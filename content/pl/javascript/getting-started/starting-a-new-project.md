---
date: 2024-01-20 18:04:00.235267-07:00
description: "How to: Zaczynanie projektu nie zawsze by\u0142o takie proste. W przesz\u0142\
  o\u015Bci, bez narz\u0119dzi takich jak npm, konfiguracja \u015Brodowiska mog\u0142\
  a zaj\u0105\u0107 godziny. Dzi\u015B,\u2026"
lastmod: '2024-04-05T21:53:37.224088-06:00'
model: gpt-4-1106-preview
summary: "Zaczynanie projektu nie zawsze by\u0142o takie proste."
title: Rozpoczynanie nowego projektu
weight: 1
---

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
