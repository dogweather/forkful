---
title:                "새 프로젝트 시작"
html_title:           "TypeScript: 새 프로젝트 시작"
simple_title:         "새 프로젝트 시작"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

새로운 프로젝트를 시작하는 것은 프로그래머들이 새로운 소프트웨어를 만들기 위해 새로운 시작을 하는 것입니다. 이것은 새로운 기능을 추가하거나 기존의 코드를 개선하기 위해 필요합니다.

## 방법:

TypeScript를 사용하여 새로운 프로젝트를 시작하는 방법은 매우 간단합니다. 먼저, 다음과 같이 TypeScript를 설치해야 합니다:
```
npm install -g typescript
```
그런 다음, 프로젝트를 시작할 디렉토리에 들어가서 다음 명령어를 실행하면 됩니다:
```
tsc --init
```
이 명령어는 tsconfig.json 파일을 생성하여 프로젝트 설정을 포함하게 됩니다. 그런 다음, TypeScript를 사용하여 코드를 작성할 수 있습니다. 예를 들어, 다음과 같이 함수를 작성할 수 있습니다:
```
function addNumbers(a: number, b: number): number {
    return a + b;
}
```
이제 TypeScript를 사용하여 작성한 코드를 JavaScript 코드로 컴파일할 수 있습니다. ``tsc`` 명령어를 실행하면 됩니다. 컴파일된 JavaScript 파일은 ``outDir`` 옵션에서 지정한 디렉토리에 생성됩니다.

## 깊이 파고들기:

TypeScript는 Microsoft에서 개발한 오픈 소스 언어로, JavaScript의 슈퍼셋입니다. 따라서 JavaScript를 향상시키는 기능을 추가하면서도 JavaScript의 모든 기능을 그대로 사용할 수 있습니다. TypeScript는 정적 타입을 지원하여 개발자가 코드를 작성하는 동안 발생할 수 있는 오류를 미리 방지할 수 있습니다. 또한 타입스크립트는 ECMAScript 표준을 준수하고 있으므로 대부분의 브라우저에서 지원됩니다. 다른 JavaScript 슈퍼셋으로는 CoffeeScript, Dart 등이 있습니다.

## 관련 자료:

- [TypeScript 공식 사이트](https://www.typescriptlang.org/)
- [TypeScript 핸드북](https://www.typescriptlang.org/docs/handbook/intro.html)
- [TypeScript 소스 코드](https://github.com/Microsoft/TypeScript)