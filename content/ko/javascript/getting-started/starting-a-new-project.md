---
date: 2024-01-20 18:04:04.553974-07:00
description: "\uC2E4\uC81C\uB85C \uD574\uBCF4\uAE30 javascript \uC2DC\uC791\uD560\
  \ \uB54C \uAE30\uBCF8\uC801\uC778 \uD2C0\uC744 \uC9DC\uBD05\uC2DC\uB2E4. \uAC00\uC7A5\
  \ \uBA3C\uC800 \uD560 \uC77C\uC740 \uC0C8\uB85C\uC6B4 \uB514\uB809\uD1A0\uB9AC\uB97C\
  \ \uB9CC\uB4E4\uACE0 \uD544\uC694\uD55C \uD30C\uC77C\uB4E4\uC744 \uCD08\uAE30\uD654\
  \uD558\uB294 \uAC70\uC5D0\uC694."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.791275-06:00'
model: gpt-4-1106-preview
summary: "javascript \uC2DC\uC791\uD560 \uB54C \uAE30\uBCF8\uC801\uC778 \uD2C0\uC744\
  \ \uC9DC\uBD05\uC2DC\uB2E4."
title: "\uC0C8 \uD504\uB85C\uC81D\uD2B8 \uC2DC\uC791\uD558\uAE30"
weight: 1
---

## 실제로 해보기
javascript 시작할 때 기본적인 틀을 짜봅시다. 가장 먼저 할 일은 새로운 디렉토리를 만들고 필요한 파일들을 초기화하는 거에요.

```javascript
// 프로젝트 디렉토리 생성
$ mkdir my-new-project
$ cd my-new-project

// package.json 파일 초기화
$ npm init -y

// 간단한 Hello World Javascript 파일 만들기
const hello = 'Hello, World!';
console.log(hello);

// 결과 출력
Hello, World!
```

## 깊게 들어가 보기
새로운 자바스크립트 프로젝트를 시작하기 전에 Node.js와 npm(노드 패키지 매니저)이 설치되어 있어야 해요. 이들은 자바스크립트가 브라우저 이외의 환경, 특히 서버 사이드에서 실행될 수 있게 만든 중요한 변화들이죠. `npm init`은 프로젝트 관리와 모듈 설치를 위한 `package.json` 파일을 만들어주는 명령어에요. 대안으로 `yarn` 이라는 도구도 있지만, npm이 더 널리 사용되고 있어요. 프로젝트 구조는 규모나 목적에 따라 다양하게 구성할 수 있지만, 일반적으로는 `src` 폴더에 소스 코드를, `public` 폴더에 정적 파일을 두는 방식이 흔해요.

## 관련 자료
- Node.js 공식 문서: [https://nodejs.org](https://nodejs.org)
- npm 공식 문서: [https://www.npmjs.com](https://www.npmjs.com)
