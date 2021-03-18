# Análise sintática descendente recursiva: Introdução

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
  
- Implementação do exemplo: [parser.predictive.g3-11](https://github.com/romildo/parser.predictive.g3-11)

- Como a execução inicia-se pelo símbolo inicial e vai fazendo as derivações à medida em que são chamadas as funções correspondentes aos símbolos no lado direito das regras de produção, temos:
  - a análise é **descendente**: a construção da árvore começa pela raiz
  - as derivações sempre são **mais à direita**
  - a análise é **preditiva**:
    - escolhe-se a regra de produção baseando-se no próximo token apenas

- Como escolher uma regra de produção quando o lado direito não apresenta um terminal explícito?
  - Resposta nas próximas aulas
