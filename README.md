# R package `stocdebt` (*english*)

This package has been developed by the [Independent Fiscal Institution (IFI) of Brazil](https://www12.senado.leg.br/ifi). It implements the stochastic component of the institution's Debt Sustainability Analysis (DSA).

Package `stocdebt` is useful to
* simulate stochastic scenarios for public debt and its determinants (real GDP growth, inflation, primary balance and interest rates);
* estimate the probability of events related to public debt.

You can read the package vignette [here](https://github.com/ifibrasil/stocdebt/releases/download/1.0.0/vignette.html).

## Installation

### Directly from GitHub

To install directly from GitHub, you can use function `install_github` from package `remotes`, for example.

```{r}
# First, install package "remotes"
install.packages("remotes")

# Second, install package "stocdebt"
remotes::install_github("ifibrasil/stocdebt/stocdebt@<VERSION>", build_vignettes = TRUE, upgrade = "never")
```

The `<VERSION>` placeholder must be replaced with one of the **stable** version numbers of the package. For example, `"ifibrasil/stocdebt/stocdebt@1.0.0"`. These numbers are on the "Releases" list of this repository (right panel of this website). In case `@<VERSION>` is omitted from the command, a **development** version (subject to errors) of the package will possibly be installed.

By the time this text is written, the latest stable version is `1.0.0`, but this number will change over time.

If you have already installed the package and want to verify the version, run

```{r}
utils::packageDescription("stocdebt")$RemoteRef
```

If the output is `"<VERSION>"` (currently, `"1.0.0"`), your installation is correct. It is the latest stable version. If the output is `"HEAD"`, you installed the development version.

Argument `build_vignettes = TRUE` is optional. If you do not use it, the package will be installed, but without an additional "User guide". Only standard documentation (help files) will be available.

Argument `upgrade = "never"` is also optional, but it simplifies the installation process.

### Installing from downloaded `.tar.gz` and `.zip` files

If you cannot install directly from GitHub, download the compressed archives available in the [release assets](https://github.com/ifibrasil/stocdebt/releases) (`stocdebt_<VERSION>.zip` for Windows and `stocdebt_<VERSION>.tar.gz` for Linux). For example, `stocdebt_1.0.0.zip` and `stocdebt_1.0.0.tar.gz`. Then, follow the [instructions to install from them](INSTALL_FROM_ARCHIVES.md).

## Methodology

The package has been used in some editions of the institution's [Fiscal Follow-Up Report](https://www12.senado.leg.br/ifi/publicacoes-ifi?tipo=relatorio), a monthly report on the fiscal stance of the Brazilian government. For more methodological details about the simulations (in portuguese), please read [Special Study n. 18](https://www2.senado.leg.br/bdsf/bitstream/handle/id/645203/EE18.pdf) (2023), [Technical Note n. 54](https://www2.senado.leg.br/bdsf/bitstream/handle/id/651183/NT54_Sensibilidade_cenarios_estocasticos_divida.pdf) (2024) and [Technical Note n. 60](LINK HERE). The latter is simply a Portuguese version of the package's vignette.

## Standard R documentation in PDF

To read the documentation of `stocdebt` in PDF form, generated according to R standard, check [stocdebt-manual.pdf](https://github.com/ifibrasil/stocdebt/releases/download/1.0.0/stocdebt-manual.pdf).

## Code improvements

Suggestions and bug reports can be submitted to ifi@senado.leg.br or to this GitHub repository.

## Integration with Python

When the package loads after the user issues `library(stocdebt)`, it looks for a Python installation in the computer (by means of its internal `.onLoad()` function). However, Python is necessary only to export charts to xlsx files. Thus, even if there is no Python installation, all other features will work appropriately.

## More about independent fiscal institutions

To learn more about national IFIs in general, visit the website of the [OECD Working Party of PBOs and IFIs](https://www.oecd.org/en/topics/parliamentary-budget-offices-and-independent-fiscal-institutions.html), or of the [EU Independent Fiscal Institutions](https://www.euifis.eu/), for example.







# Pacote R `stocdebt` (*português*)

Este pacote foi desenvolvido pela [Instituição Fiscal Independente (IFI)](https://www12.senado.leg.br/ifi) brasileira. Ele implementa a parte estocástica da análise de sustentabilidade da dívida (*Debt Sustainability Analysis* - DSA) da instituição.

O pacote `stocdebt` é útil para
* simular cenários estocásticos para a dívida pública e seus determinantes (crescimento real do PIB, inflação, resultado primário e taxa de juros);
* estimar a probabilidade de eventos relacionados à dívida pública.

Leia a *vignette* do pacote [aqui](https://github.com/ifibrasil/stocdebt/releases/download/1.0.0/vignette.html).

## Instalação

### Diretamente do GitHub

Para instalar diretamente do GitHub, pode-se usar a função `install_github` do pacote `remotes`, por exemplo.

```{r}
# Primeiro, instale o pacote "remotes"
install.packages("remotes")

# Segundo, instale o pacote "stocdebt"
remotes::install_github("ifibrasil/stocdebt/stocdebt@<VERSÃO>", build_vignettes = TRUE, upgrade = "never")
```

O trecho `<VERSÃO>` deve ser substituído por um dos números de versão **estável** do pacote, que constam na lista de "Versões" (ou "*Releases*") deste repositório (painel direito desta página web). Por exemplo, `"ifibrasil/stocdebt/stocdebt@1.0.0"`. Caso `@<VERSÃO>` seja omitido do comando, possivelmente será instalada uma versão em **desenvolvimento** do pacote, que está sujeita a falhas.

No momento em que este texto é escrito, a versão estável mais recente é a `1.0.0`, mas a numeração mudará conforme o tempo passar.

Se o(a) usuário(a) já instalou o pacote e deseja verificar a versão, basta executar

```{r}
utils::packageDescription("stocdebt")$RemoteRef
```

Se o resultado for `"<VERSÃO>"` (no caso atual, `"1.0.0"`), a instalação está correta. É a versão estável mais recente. Se o resultado for `"HEAD"`, trata-se da versão em desenvolvimento.

O argumento `build_vignettes = TRUE` é opcional. Se o(a) usuário(a) não utilizá-lo, o pacote será instalado sem um guia de usuário ("*User guide*"). Apenas a documentação padrão (*help files*) estará disponível.

O argumento `upgrade = "never"` também é opcional, mas simplifica o processo de instalação.

### Instalação a partir dos arquivos `.tar.gz` e `.zip` baixados

Se não for possível instalar diretamente do GitHub, baixe os arquivos anexos à [versão de interesse](https://github.com/ifibrasil/stocdebt/releases). Os arquivos são `stocdebt_<VERSÃO>.zip` (para Windows) e `stocdebt_<VERSÃO>.tar.gz` (para Linux). Por exemplo, `stocdebt_1.0.0.zip` e `stocdebt_1.0.0.tar.gz`. Depois, siga as [instruções para instalação via arquivos](INSTALL_FROM_ARCHIVES.md).

## Metodologia

O pacote foi usado para elaborar algumas edições do [Relatório de Acompanhamento Fiscal](https://www12.senado.leg.br/ifi/publicacoes-ifi?tipo=relatorio) da instituição, um documento mensal sobre a situação fiscal do governo brasileiro.Para mais detalhes metodológicos sobre as simulações, veja o [Estudo Especial n. 18](https://www2.senado.leg.br/bdsf/bitstream/handle/id/645203/EE18.pdf) (2023), a [Nota Técnica n. 54](https://www2.senado.leg.br/bdsf/bitstream/handle/id/651183/NT54_Sensibilidade_cenarios_estocasticos_divida.pdf) (2024) e a [Nota Técnica n. 60](LINK HERE). Esta última é simplesmente a versão em português da *vignette* que acompanha o pacote.

## Documentação em PDF no padrão R

A documentação em PDF do pacote `stocdebt`, gerado conforme o padrão do R, pode ser consultado em [stocdebt-manual.pdf](https://github.com/ifibrasil/stocdebt/releases/download/1.0.0/stocdebt-manual.pdf).

## Aprimoramentos do código

Sugestões e *bug reports* podem ser enviados para ifi@senado.leg.br ou para este repositório do GitHub.

## Integração com Python

Ao ser carregado pelo(a) usuário(a) no R, por meio do comando `library(stocdebt)`, o pacote busca por uma instalação do Python no computador. Essa busca é feita pela função `.onLoad()`, interna ao pacote. O Python é necessário apenas para exportar gráficos para arquivos xlsx. Todas as demais operações funcionarão adequadamente, mesmo que não haja uma instalação de Python no computador.

## Mais sobre instituições fiscais independentes

Para saber mais sobre IFIs nacionais em geral, visite o site do [OECD Working Party of PBOs and IFIs](https://www.oecd.org/en/topics/parliamentary-budget-offices-and-independent-fiscal-institutions.html), ou da rede das [EU Independent Fiscal Institutions](https://www.euifis.eu/), por exemplo.
