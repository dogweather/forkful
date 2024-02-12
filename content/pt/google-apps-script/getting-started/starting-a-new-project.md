---
title:                "Iniciando um novo projeto"
aliases:
- /pt/google-apps-script/starting-a-new-project/
date:                  2024-02-01T22:02:48.381222-07:00
model:                 gpt-4-0125-preview
simple_title:         "Iniciando um novo projeto"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/google-apps-script/starting-a-new-project.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Iniciar um novo projeto no Google Apps Script (GAS) envolve a inicialização de um arquivo de script dentro do ecossistema do Google (Google Drive, Docs, Sheets, etc.) para automatizar tarefas ou estender as funcionalidades dos Google Apps. Programadores frequentemente embarcam nesta jornada para agilizar fluxos de trabalho, manipular serviços do Google programaticamente, ou criar complementos personalizados, economizando tempo e aproveitando o poder da infraestrutura do Google.

## Como:

Para dar início a um novo projeto no Google Apps Script, você tem alguns pontos de entrada, mas vamos nos concentrar no método mais direto: criando um script a partir do Google Drive.

1. **Criando um Projeto no Google Drive**
   - Navegue até o Google Drive (drive.google.com).
   - Clique em “+ Novo” > “Mais” > “Google Apps Script”.
   - Um novo projeto de script é aberto no editor. Por padrão, ele contém um arquivo `Code.gs` com uma amostra da `myFunction`.

2. **Configurando Seu Projeto**
   - Renomeie seu projeto para mais clareza. Clique em “Projeto sem título” no topo à esquerda, e dê a ele um nome significativo.
   - Escreva uma função simples no arquivo `Code.gs` para ter uma ideia:

```javascript
function helloWorld() {
  Logger.log('Olá, mundo!');
}
```

   - Execute `helloWorld` selecionando a função no dropdown ao lado do botão play (▶) e clicando nele. Isso executará a função.

3. **Visualizando Logs**
   - Para visualizar a saída do `Logger.log`, vá em “Ver” > “Logs”, ou pressione `Ctrl + Enter`. Você verá "Olá, mundo!" nos logs.

Parabéns, você acaba de iniciar com sucesso um novo projeto no Google Apps Script e executou uma função simples!

## Aprofundando

A criação do Google Apps Script por volta de 2009 forneceu uma plataforma poderosa, mas acessível, tanto para desenvolvedores quanto para não desenvolvedores, para automatizar, estender e construir em cima da vasta gama de serviços do Google. Diferente dos ambientes de programação tradicionais, o GAS oferece uma mistura única de simplicidade e integração, diretamente dentro do ecossistema do Google, sem a necessidade de servidores externos ou configuração. Este modelo de execução sem servidor simplifica enormemente a implantação e gerenciamento de projetos.

Historicamente, o GAS era um tanto limitado por seu ambiente de execução e versão da linguagem, muitas vezes ficando atrás dos padrões atuais do JavaScript. No entanto, atualizações recentes trouxeram a sintaxe moderna do JavaScript (ECMAScript 2015+) para o GAS, tornando-o mais palatável para desenvolvedores acostumados às práticas de desenvolvimento contemporâneo.

Embora o GAS esteja singularmente posicionado para interagir com os Serviços Google, existem abordagens alternativas para necessidades mais intensivas ou específicas. Por exemplo, o Google Cloud Functions e o Google Cloud Platform (GCP) oferecem soluções mais robustas e escaláveis para lidar com fluxos de trabalho complexos, processar grandes conjuntos de dados e integrar com APIs externas. Essas plataformas permitem a programação em várias linguagens (por exemplo, Python, Go, Node.js) e oferecem maiores recursos computacionais.

No entanto, para tarefas intrinsecamente ligadas aos Google Apps, automação e desenvolvimento rápido dentro deste ecossistema, o Google Apps Script permanece uma ferramenta inigualável em termos de facilidade de uso e profundidade de integração. Sua acessibilidade diretamente do Google Drive e conexão contínua com os serviços do Google tornam-no uma escolha prática para uma ampla gama de projetos, particularmente para aqueles que buscam estender a funcionalidade de Sheets, Docs, Forms e outros aplicativos do Google.
