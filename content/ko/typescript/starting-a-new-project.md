---
title:                "새로운 프로젝트 시작하기"
html_title:           "TypeScript: 새로운 프로젝트 시작하기"
simple_title:         "새로운 프로젝트 시작하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 왜
새 프로젝트를 시작하는 이유는 여러 가지가 있을 수 있습니다. 예를 들어, 새로운 기술을 배우고 익히기 위해서, 또는 새로운 기능을 구현하여 더 나은 제품을 출시하기 위해서 등이 있을 수 있습니다. 이 글에서는 TypeScript를 이용하여 새로운 프로젝트를 시작하는 방법을 알아보겠습니다.

## 어떻게
TypeScript를 이용하여 새로운 프로젝트를 시작하기 위해서는 먼저 TypeScript를 설치해야 합니다. 일반적으로는 Node.js를 이용하여 NPM을 통해 TypeScript를 설치할 수 있습니다.

```TypeScript
npm install typescript --save-dev
```

설치가 완료되면, 프로젝트 폴더 안에서 `tsc --init` 명령어를 실행하여 프로젝트의 tsconfig 파일을 생성할 수 있습니다.

```TypeScript
tsc --init
```

tsconfig 파일을 올바르게 설정하면, TypeScript 코드를 컴파일하여 JavaScript로 변환할 수 있습니다.

```TypeScript
// app.ts 
console.log("Hello World!");

// Compile
tsc app.ts

// Output: app.js
console.log("Hello World!");
```

TypeScript는 정적 타입 언어이기 때문에 변수의 타입을 선언해주어야 합니다. 타입을 지정하면 코드의 가독성을 높일 수 있고, 버그를 사전에 방지할 수 있습니다.

```TypeScript
// app.ts
const age: number = 25;
const name: string = "John";

console.log(`Hi, my name is ${name} and I am ${age} years old.`);

// Compile
tsc app.ts

// Output: app.js
var age = 25;
var name = "John";
console.log("Hi, my name is " + name + " and I am " + age + " years old.");
```

## 딥 다이브
새로운 프로젝트를 시작할 때 딥 다이브할 가치가 있는 몇 가지 추가 정보들이 있습니다. 첫째, TypeScript는 자바스크립트와 호환성이 높기 때문에 이미 익숙한 자바스크립트 문법을 그대로 사용할 수 있습니다. 둘째, 타입 스크립트를 사용하면 코드의 가독성을 높일 수 있고, 점진적으로 타입을 추가하여 안정성을 확보할 수 있습니다. 셋째, TypeScript는 강력한 타입 추론 기능을 지원하기 때문에 타입을 별도로 명시하지 않아도 됩니다. 넷째, 다양한 IDE나 코드 에디터에서 TypeScript를 지원하기 때문에 개발 툴에 대한 선택의 폭이 넓습니다.

## 관련 링크
- [TypeScript 공식 홈페이지](https://www.typescriptlang.org/)
- [TypeScript 시작하기 - 한국어 번역](https://typescript-kr.github.io/pages/tutorials/TypeScript%20%EC%8B%9C%EC%9E%91%ED%95%98%EA%B8%B0.html)
- [타입스크립트 핸드북 - 한국어 번역](https://typescript-kr.github.io/)

## 참고 링크
- [TypeScript for Beginners: Getting Started](https://dev.to/bitovi/typescript-for-beginners-getting-started-3klb)
- [Starting a new project with TypeScript](https://levelup.gitconnected.com/starting-a-new-project-with-typescript-52088d0d4add)
- [Why You Should Consider Adopting TypeScript](https://codeburst.io/why-you-should-consider-adopting-typescript-d7dd39281292)