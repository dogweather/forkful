---
title:                "Rozpoczynanie nowego projektu"
date:                  2024-01-20T18:04:00.235267-07:00
model:                 gpt-4-1106-preview
simple_title:         "Rozpoczynanie nowego projektu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Zaczynanie nowego projektu to stawianie pierwszych kroków w kodzie. Robimy to, by rozwiązać problem, nauczyć się czegoś nowego lub stworzyć coś wartościowego.

## How to:

```Javascript
// Step 1: Initialize npm (Node Package Manager)
// Type in the terminal inside your project's folder
npm init -y

// This creates a package.json file with default values.

// Step 2: Install a package (e.g., Express.js for a web server)
npm install express --save

// This adds Express to your dependencies in package.json.

// Step 3: Create your main file (e.g., app.js)
// Write basic server code:

const express = require('express');
const app = express();

app.get('/', (req, res) => {
  res.send('Hello, World!');
});

app.listen(3000, () => {
  console.log('Server is running on http://localhost:3000');
});

// Step 4: Run your application
// Type in the terminal
node app.js

// Sample output should be: "Server is running on http://localhost:3000"
```

## Deep Dive:

Zaczynanie projektu nie zawsze było takie proste. W przeszłości, bez narzędzi takich jak npm, konfiguracja środowiska mogła zająć godziny. Dziś, npm i inne menedżery pakietów jak Yarn upraszczają ten proces.

Alternatywą do npm może być Yarn, który działa podobnie, ale jest szybszy i oferuje lepsze zarządzanie wersjami pakietów. Przy nowym projekcie warto rozważyć też TypeScript dla lepszej typizacji, czy frameworki jak Angular, React czy Vue.js - zależnie od zastosowania.

Kluczowym aspektem jest jednak nie tylko wybranie narzędzi, ale zrozumienie, jak i dlaczego działają, co ułatwia debugowanie i rozwój projektu.

## See Also:

- [npm documentation](https://docs.npmjs.com/)
- [Node.js official site](https://nodejs.org/en/)
- [Express.js guide](https://expressjs.com/en/starter/installing.html)
- [Yarn package manager](https://yarnpkg.com/)
- [TypeScript basics](https://www.typescriptlang.org/docs/handbook/typescript-from-scratch.html)
- [React documentation](https://reactjs.org/docs/getting-started.html)
- [Angular's official website](https://angular.io/)
- [Vue.js guide](https://vuejs.org/v2/guide/)