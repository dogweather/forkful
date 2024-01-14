---
title:                "Javascript: 텍스트 파일 작성하기"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 작성하는 것은 프로그래밍에서 중요한 부분입니다. 이것은 다양한 목적으로 사용될 수 있고 데이터를 다루는 많은 방법 중 하나입니다. 만약 당신이 텍스트 파일을 작성하는 방법을 아직 모른다면 이 글을 계속 읽어보세요!

## 어떻게

텍스트 파일을 작성하는 가장 간단한 방법은 `writeFileSync()` 함수를 사용하는 것입니다. 이 함수는 Node.js에서 제공되는 기능으로, 파일 경로와 데이터를 인자로 받아 파일을 생성하고 데이터를 쓸 수 있게 해줍니다. 아래의 예제를 참고하세요.

```Javascript
fs.writeFileSync('output.txt', 'Hello, world!');

```

이 코드를 실행하면 현재 디렉토리에 `output.txt`이라는 이름의 파일이 생성되고 "Hello, world!"라는 내용이 쓰여집니다.

## 딥 다이브

텍스트 파일을 작성하는 것은 결국 데이터를 저장하는 것입니다. 이때 데이터 포맷이 중요해지는데, 일반적으로 텍스트 파일은 간단한 문자열로 구성되어 있습니다. 그리고 그 문자열은 여러 줄로 이뤄져 있을 수 있고 그 안에는 다양한 정보가 들어갈 수 있습니다. 예를 들어, CSV 파일은 쉽게 만들 수 있고 여러 데이터를 포함시킬 수 있습니다.

하지만 앞서 말한 `writeFileSync()` 함수는 단순한 예제이므로 중요한 것은 데이터 포맷보다는 파일을 만드는 방법과 데이터를 어떻게 쓰느냐에 대해 이해하는 것입니다.

## 더 알아보기

텍스트 파일을 작성하는 것 외에도 Node.js에서는 다양한 파일 작업 기능을 제공합니다. 파일을 읽고 쓰는 방법을 익히는 것은 프로그래밍에서 매우 중요한 부분이기 때문에 관련 정보를 더 찾아보는 것을 추천합니다.

### 이것도 참고하세요

- [Node.js fs 모듈 문서](https://nodejs.org/api/fs.html)
- [텍스트 파일 쓰기 - 노마드코더 유튜브 강의](https://youtu.be/RuztJ4phU_Y)
- [파일 다루기 - 드림코딩 엘리스 유튜브 강의](https://youtu.be/T7cDYFiz4lk)