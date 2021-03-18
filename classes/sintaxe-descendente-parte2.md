# Análise sintática descendente recursiva

- A implementação do analisador sintático é feita com um conjunto de **funções mutuamente recursivas**.

- Faz-se **uma função para cada símbolo não terminal**.

- Esta função analisa se a cadeia de entrada é derivada do não terminal.

- Na **implementação** da função:
  - verificar qual é o próximo token da entrada
  - escolher uma das regras de produção do não terminal cujo lado direito começa com este token
  - verificar cada um dos símbolos no lado direito da regra de produção escolhida
    - terminal:
      - o próximo terminal da entrada tem que ser do tipo indicado na regra
      - caso não seja, reportar erro
    - não-terminal:
      - chamar a função correspondente para reconhecer uma cadeia derivada do não terminal
  - a função **principal** é a função correspondente ao símbolo inicial da gramática
      
- Exemplo: gramática 3.11 do livro do Appel

  _S_ → `if` _E_ `then` _S_ `else` _S_
  _S_ → `begin` _S_ _L_
  _S_ → `print` _E_

  _L_ → `end`
  _L_ → `;` _S_ _L_

  _E_ → `num` `=` `num`

- Como a execução inicia-se pelo símbolo inicial e vai fazendo as derivações à medida em que são chamadas as funções correspondentes aos símbolos no lado direito das regras de produção, temos:
  - a análise é **descendente**: a construção da árvore começa pela raiz
  - as derivações sempre são **mais à direita**
  - a análise é **preditiva**:
    - escolhe-se a regra de produção baseando-se no próximo token apenas

- Como escolher uma regra de produção quando o lado direito não apresenta um terminal explícito?

# *Nullable*

- Um símbolo não terminal é **anulável** quando ele pode derivar a cadeia vazia.
  - ou seja, ele pode ser _apagado_ em uma sequência de derivações
  
- Um símbolo não terminável é **anulável** se houver alguma regra para ele one
  - o lado direito é a cadeia vazia
  - o lado direito é formado apenas por símbolos não terminais anuláveis
  
- Um símbolo terminal não é anulável.

- Uma cadeia é anulável se todos os seus símbolos são anuláveis.

- O cálculo pode ser feito de forma construtiva para os não terminais:
  - inicialmente considera-se que todos os não termináveis não são anuláveis
  - faz-se sucessivas passagens pelo conjunto de regras de produção, até que não haja mais alterações nos resultados
    - se uma das condições acima for satisfeita, o símbolo do lado esquerdo da regra é considerado anulável

- Exemplo:

      _Z_ → `d`
      _Z_ → _X_ _Y_ _Z_

      _Y_ →
      _Y_ → `c`

      _X_ → _Y_
      _X_ → `a`
  

  NT | Nullable
  ---+---------
  _X_| não
  _Y_| não
  _Z_| não
  ---+---------

# Conjunto *FIRST*

- O conjunto **FIRST** de um não terminal é o conjunto de todos os terminais que podem começar uma cadeia derivada do não terminal.

- O conjunto **FIRST** de uma cadeia é o conjunto de todos os terminais que podem começar uma cadeia derivada da cadeia dada.

- O cálculo pode ser feito de forma construtiva para os não terminais:
  - inicialmente os conjuntos **FIRST** são todos vazios
  - faz-se sucessivas passagens pelo conjunto de regras de produção, até que não haja mais alterações nos resultados
    - se o lado direito começa com um terminal, esse terminal é adicionado ao conjunto **FIRST** do nao terminal do lado esquerdo  
      _A_ → `t` _B_ _C_         FIRST(_A_) := FIRST(_A_) U {`t`}
    - se o lado direito começa com um não terminal, todos os elementos do **FIRST** desse não terminal são adicionados ao **FIRST** do não terminal que está no lado esquerdo da regra
      _A_ → _B_ _C_         FIRST(_A_) := FIRST(_A_) U FIRST(_B_)
      - Se o não terminal no começo do lado direito for anulável, deve-se considerar o próximo símbolo
        _A_ → _B_ _C_ _D_        FIRST(_A_) := FIRST(_A_) U FIRST(_B_), se _A_ for anulável
        - Este raciocínio deve ser empregado enquanto se tiver uma sequência de não terminais anuláveis no começo do lado direito da regra
    
    

- Exemplo:

      _Z_ → `d`
      _Z_ → _X_ _Y_ _Z_

      _Y_ →
      _Y_ → `c`

      _X_ → _Y_
      _X_ → `a`
  

  NT | Nullable | FIRST
  ---+----------+------
  _X_| sim      |      
  _Y_| sim      |      
  _Z_| não      |      
  ---+----------+------

# Conjunto *FOLLOW*

- O conjunto **FOLLOW** de um não terminal é o conjunto de todos os terminais que podem vir imediatamente após o não terminal em uma cadeia.

- Exmplo:
  - _Z_ => _X_ _Y_ _Z_ => _X_ `c` _Z_
  - Nesta cadeia o terminal `c` aparece imediatamente após o não terminal `_X_`, logo `c` pertence FOLLOW(_X_)

- O cálculo pode ser feito de forma construtiva:
  - inicialmente os conjuntos **FOLLOW** são todos vazios
  - faz-se sucessivas passagens pelo conjunto de regras de produção, até que não haja mais alterações nos resultados
    - para cada não terminal no lado direito da regra
      - se ele for seguido de um terminal, este terminal é adicionado ao FOLLOW do não terminal
        _A_ → _B_ `t` _C_         FOLLOW(_B_) := FOLLOW(_B_) U {`t`}
        
      - se ele for seguido de um outro não terminal, o FIRST deste outro não terminal deve ser adicionado ao FOLLOW do primeiro terminal
        _A_ → _B_ _C_ _D_        FOLLOW(_B_) := FOLLOW(_B_) U FOLLOW(_C_)

        - se esse outro não terminal for anulável, deve se considerar os próximos símbolos
          _A_ → _B_ _C_ _D_       FOLLOW(_B_) := FOLLOW(_B_) U FOLLOW(_C_) U FOLLOW(_D_), se _C_ for anulável
    
    - se o lado direito da regra termina com um não terminal,
      - ao FOLLOW desse não terminal deve ser acrescentado o follow do não terminal que está no lado esquerdo da regra de produção
        _A_ → _B_ _C_ _D_        FOLLOW(_D_) := FIRST(_D_) U FOLLOW(_A_)
        
      - se esse não terminal for anulável, considera-se a possibilidade de que ele pode ser apagado, e aplica-se essa regra ao não terminal que esteja imediatamente antes dele (se houver)
        _A_ → _B_ _C_ _D_        FOLLOW(_C_) := FIRST(_C_) U FOLLOW(_A_), se _D_ for anulável

- Exemplo:

      _Z_ → `d`
      _Z_ → _X_ _Y_ _Z_

      _Y_ →
      _Y_ → `c`

      _X_ → _Y_
      _X_ → `a`
  

  NT | Nullable | FIRST
  ---+----------+------
  _X_| sim      |      
  _Y_| sim      |      
  _Z_| não      |      
  ---+----------+------
