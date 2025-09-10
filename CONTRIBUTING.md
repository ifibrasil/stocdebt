# Contributing to `stocdebt` development (*english*)

Other organizations, fiscal councils and independent fiscal institutions are welcome to use, share and contribute to package `stocdebt`.

Suggestions and bug reports can be submitted to ifi&#64;senado&#46;leg&#46;br or through this GitHub repository.

## Improvements/extensions to the package

The code can be improved in several ways. These are some possible enhancements:

* **probability of more events**: extend the list of events of interest, whose probability is estimated by function `event_prob`;
* **better printing of function outputs**: for example, create a table, to be printed to the terminal, with the estimates returned by `event_prob`, where each rows would contain the description of the event and its numerical estimate; 
* **validation tests**: develop unit tests and add more error handlers to the functions;
* **additional strategies to generate shocks**: function `shocks_generator` has been written so that it could be easily extended, accepting new `if` blocks, which are selected according to argument `case`.

Note that most functions of the package return a `list`. This means that, if a developer wants to extend a function so that a new piece of output is returned, he/she can simply add the new object to that `list`. By doing so, the rest of the package will continue to work properly.

### Namespaces to avoid errors

When writing this R code, many functions from other packages have been used. To ensure that the correct functions were invoked at each code line, all of them have been preceded by their namespaces in the form `package::function`. For example, we use `base::colnames(x)` instead of simply `colnames(x)`, or `zoo::index(x)` instead of `index(x)`.

Even though such choice makes the code lengthier, it avoids errors derived from overlapping functions with the very same name, but belonging to different packages.

### Classes and methods

The code has been developed without creating new classes and methods of R. The aim of this decision was to make the code understandable by a wider audience. However, as a consequence, there are more conditions (`if`) internally, across the functions. Insofar as the amount of conditioning is tractable, we do not intend to use classes and methods.

### Efficiency

There will always be room for improvement in efficiency, that is, faster simulations. To those developers willing to propose more efficient code, we suggest that they ensure the new version is not difficult to understand for a broad audience. For example, some loops could be replaced with vector operations, but such replacement may make the role of the code chunk less evident in the overall computations executed by a function.







# Contribuindo para o desenvolvimento de `stocdebt` (*português*)

Outras organizações, conselhos fiscais e instituições fiscais independentes são convidadas a usar, compartilhar e contribuir para o pacote `stocdebt`.

Sugestões e *bug reports* podem ser enviados para ifi@senado.leg.br ou para este repositório do GitHub.

## Aprimoramentos/extensões ao pacote

O código pode ser aprimorado de diversas formas. Entre as possibilidades, tem-se:

* **probabilidade de mais eventos**: estender a lista de eventos de interesse, cuja probabilidade é estimada pela função `event_prob`;
* **melhor exibição (*print*) dos resultados**: por exemplo, criar uma tabela, a ser exibida no terminal, contendo as estimativas retornadas por `event_prob`, em que cada linha apresenta a descrição do evento e a estimativa numérica; 
* **testes de validação**: elaborar testes unitários (*unit tests*) e mais tratamentos de erro (*error handlers*);
* **mais estratégias de geração de choques**: a função `shocks_generator` foi elaborada de modo que pudesse ser facilmente estendida, pois aceita novos blocos condicionais (`if`), acionados pelo argumento `case`.

Note que a maioria das funções retorna um objeto do tipo `list`. Isso significa que, se um desenvolvedor deseja estender uma função específica de forma que um novo objeto seja retornado pela função, ele(a) pode simplesmente adicionar esse objeto àquela `list` final da função. Assim, o restante do pacote continuará a funcionar adequadamente.

### Namespaces para evitar erros

Ao elaborar esse código, diversas funções de outros pacotes foram utilizadas. Para garantir que as funções corretas fossem acessadas, todas foram precedidas or seus *namespaces*, no formato `pacote::função`. Por exemplo, usou-se `base::colnames(x)` no lugar de `colnames(x)`, ou ainda, `zoo::index(x)` no lugar de `index(x)`.

Muito embora essa escolha tenha tornado o código mais longo, evita-se erros decorrentes da troca desavisada entre funções que possuem o mesmo nome, mas pertencem a pacotes distintos.

### Classes e métodos

Optou-se por elaborar o código sem criar novas classes e métodos do R. O objetivo dessa escolha é tornar o código compreensível por uma audiência mais ampla. Contudo, como consequência, há maior uso de condicionais (`if`) internamente, através das funções. Enquanto a quantidade de condicionais for tratável, não se pretende recorrer à criação de classes e métodos.

### Eficiência

Sempre há espaço para aprimoramento da eficiência, ou seja, da velocidade das simulações mais rápidas. Para os desenvolvedores que desejarem propor um código mais eficiente, sugere-se que a nova versão não seja difícil de compreender por uma audiência mais ampla. Por exemplo, alguns *loops* poderiam de fato ser substituídos por operações vetoriais. Entretanto, tais substituições podem tornar menos evidente o papel que a parte editada da função desempenha na função como um todo.
