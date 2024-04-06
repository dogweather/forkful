---
date: 2024-01-20 18:04:43.115131-07:00
description: "How to (\uC5B4\uB5BB\uAC8C \uD558\uB098) \uC0C8\uB85C\uC6B4 TypeScript\
  \ \uD504\uB85C\uC81D\uD2B8\uB97C \uC2DC\uC791\uD560 \uB54C, \uC774\uB294 \uB2F9\uC2E0\
  \uC774 \uC811\uADFC \uBC29\uC2DD\uC5D0 \uC788\uC5B4\uC11C \uC644\uC804\uD55C \uC790\
  \uC720\uB97C \uC9C0\uB2C8\uAC8C \uB428\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC77C\
  \uBC18\uC801\uC73C\uB85C TypeScript \uD504\uB85C\uC81D\uD2B8\uB294 `tsconfig.json`\
  \ \uD30C\uC77C\uC744 \uD1B5\uD574 \uC124\uC815\uC744 \uAD00\uB9AC\uD558\uBA70, `src`\
  \ \uD3F4\uB354\uC5D0\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.658618-06:00'
model: gpt-4-1106-preview
summary: "\uC77C\uBC18\uC801\uC73C\uB85C TypeScript \uD504\uB85C\uC81D\uD2B8\uB294\
  \ `tsconfig.json` \uD30C\uC77C\uC744 \uD1B5\uD574 \uC124\uC815\uC744 \uAD00\uB9AC\
  \uD558\uBA70, `src` \uD3F4\uB354\uC5D0 TypeScript \uC18C\uC2A4 \uCF54\uB4DC\uB97C\
  \ \uBCF4\uAD00\uD558\uB294 \uAC83\uC774 \uAD00\uB840\uC785\uB2C8\uB2E4."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## How to (어떻게 하나)
```TypeScript
// TypeScript의 최신 버전을 사용하여 새 프로젝트 설정하기

// 1. 필요한 경우 Node.js와 npm을 설치합니다.

// 2. 터미널에서 아래 명령어를 사용해 TypeScript를 전역으로 설치합니다.
npm install -g typescript

// 3. 새 프로젝트 디렉토리를 만듭니다.
mkdir my-new-project
cd my-new-project

// 4. npm을 통해 새 프로젝트를 시작합니다.
npm init -y

// 5. TypeScript 설정 파일 생성합니다.
tsc --init

// 6. src 폴더와 기본 TypeScript 파일을 만듭니다.
mkdir src
echo "console.log('Hello, TypeScript!');" > src/index.ts

// 7. TypeScript를 컴파일합니다.
tsc

// 샘플 출력
// 디렉토리 안에 'dist' 폴더가 생성되며, 'index.js' 파일 안에 JavaScript 코드가 포함됩니다.
```

## Deep Dive (심층 분석)
새로운 TypeScript 프로젝트를 시작할 때, 이는 당신이 접근 방식에 있어서 완전한 자유를 지니게 됨을 의미합니다. 일반적으로 TypeScript 프로젝트는 `tsconfig.json` 파일을 통해 설정을 관리하며, `src` 폴더에 TypeScript 소스 코드를 보관하는 것이 관례입니다. 이후 컴파일 시 `dist`나 `build` 폴더 내에 JavaScript 코드가 생성됩니다.

역사적인 맥락에서, TypeScript는 2012년 마이크로소프트에 의해 발표되었으며, JavaScript에 타입 안정성을 추가함으로써 대규모 애플리케이션을 더 쉽게 구축하고 유지할 수 있게 했습니다. 대안으로는 Flow, Dart, 그리고 Elm 등이 있으나, TypeScript는 강력한 커뮤니티 지원과 무난한 JavaScript 통합 덕분에 널리 채택되었습니다.

TypeScript 프로젝트를 시작할 때 중요한 구현 세부 사항은 다음과 같습니다:
- `tsconfig.json` 파일을 통한 컴파일러 옵션 설정
- 필요한 경우 `type`과 `interface`를 통한 복잡한 타입 정의
- `npm` 혹은 `yarn`과 같은 패키지 매니저를 사용하여 의존성 관리
- `eslint` 혹은 `prettier`를 통한 코드 스타일 및 포맷팅 규칙 설정

## See Also (더 보기)
- TypeScript 공식 웹사이트: [https://www.typescriptlang.org/](https://www.typescriptlang.org/)
- TypeScript GitHub 저장소: [https://github.com/Microsoft/TypeScript](https://github.com/Microsoft/TypeScript)
- npm TypeScript 패키지 페이지: [https://www.npmjs.com/package/typescript](https://www.npmjs.com/package/typescript)
- tsconfig.json 문서: [https://www.typescriptlang.org/docs/handbook/tsconfig-json.html](https://www.typescriptlang.org/docs/handbook/tsconfig-json.html)
- TypeScript 커뮤니티: [https://discord.gg/typescript](https://discord.gg/typescript)
