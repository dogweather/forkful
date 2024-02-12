---
title:                "새 프로젝트 시작하기"
aliases:
- /ko/javascript/starting-a-new-project.md
date:                  2024-01-20T18:04:04.553974-07:00
model:                 gpt-4-1106-preview
simple_title:         "새 프로젝트 시작하기"

tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 시작은 뭘까? 왜 해야 할까?
새 프로젝트를 시작한다는 건 깨끗한 캔버스에 그림을 그리는 것 같아요. 개발자들은 새로운 아이디어나 해결책을 현실로 만들기 위해서 프로젝트를 시작해요.

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
