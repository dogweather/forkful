---
title:                "새 프로젝트 시작하기"
html_title:           "Arduino: 새 프로젝트 시작하기"
simple_title:         "새 프로젝트 시작하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

새 프로젝트를 시작하는 것은 사실상 새로운 제품이나 기능를 만드는 데 필요한 코드의 컬렉션을 만드는 과정입니다. 프로그래머들은 새로운 문제를 해결하거나 신규 기능을 개발하거나, 개선하려는 목적으로 이를 수행합니다.

## 실행 방법:

새 프로젝트를 만드는 가장 간단한 방법은 `tsc --init` 명령을 이용하는 것입니다. 이 명령을 실행하면 TypeScript 프로젝트를 위한 기본 `tsconfig.json` 파일이 생성됩니다.

```TypeScript
$ tsc --init
message TS6071: Successfully created a tsconfig.json file.
```

그런다음, `src` 폴더를 만들고, 그 안에 우리의 첫 번째 TypeScript 파일 `app.ts`를 생성해보겠습니다.

```TypeScript
$ mkdir src
$ echo "console.log('Hello, TypeScript!')" > src/app.ts
```

이제 TypeScript 코드를 컴파일하면 JavaScript 파일이 생성됩니다.

```TypeScript
$ tsc
```

이제 `app.js` 파일을 실행해 보겠습니다.

```TypeScript
$ node src/app.js
Hello, TypeScript!
```

## 심층 분석:

시작하는 새 프로젝트는 내부 시스템, 기능, 또는 서비스의 개발의 대부분을 구성합니다. 원래 이것은 많은 설정과 준비작업이 필요했지만, TypeScript와 같은 프레임워크의 등장으로 보다 쉽고 빠르게 시작할 수 있게 되었습니다. 그러나 필요에 따라 다른 도구 및 프레임워크도 선택할 수 있습니다. 프로젝트 실행을 위해 Angular, React 같은 프레임워크를 사용할 수도 있습니다. 프로젝트 설정에 따라 필요한 패키지 있форм과 구성을 결정할 수 있습니다. 본질적으로 프로젝트를 시작하는 방법은 문제와 요구 사항에 따라 달라집니다.

## 참고 자료:

- TypeScript 공식 문서: [TypeScript Tutorial](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)
- TypeScript 프로젝트 설정: [Get started with TypeScript](https://www.digitalocean.com/community/tutorials/typescript-new-project)
- TypeScript와 JavaScript의 차이: [What’s the difference?](https://www.freecodecamp.org/news/what-s-the-difference-between-javascript-and-typescript-4b0b38e2ebc0/)