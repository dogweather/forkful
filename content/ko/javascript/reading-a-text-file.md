---
title:                "텍스트 파일 읽기"
html_title:           "Javascript: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

텍스트 파일을 읽는 것은 컴퓨터 프로그래밍에서 일반적인 작업입니다. 이 작업은 텍스트 파일 안에 저장된 데이터를 읽어서 우리가 원하는 정보를 파악하는 데 사용됩니다. 프로그래머들은 이 작업을 통해 필요한 정보를 가져와서 프로그램을 만들고, 수정하는 데 도움을 줍니다.

## 하는 방법:

```Javascript
const fs = require('fs'); // Node.js에서 파일 시스템 모듈을 사용하기 위해 require문을 사용합니다.

let data = fs.readFileSync('textfile.txt', 'utf-8'); // 텍스트 파일을 읽고, 읽은 내용을 변수에 저장합니다.

console.log(data); // 변수에 저장된 내용을 콘솔에 출력합니다.
```

위 예시를 통해 파일 시스템 모듈을 불러오고, 파일을 읽은 내용을 변수에 저장하고, 그 내용을 콘솔에 출력하는 방법을 알 수 있습니다. 이 외에도 다양한 방법으로 파일을 읽을 수 있으며, 읽은 내용을 다양한 방식으로 활용할 수 있습니다.

## 더 들어가기:

텍스트 파일을 읽는 작업은 컴퓨터의 발전과 함께 진행된 작업입니다. 이전에는 텍스트 파일을 읽기 위해 많은 노력이 필요했지만, 지금은 간단한 코드 몇 줄만으로도 읽을 수 있습니다. 이 외에도 JSON 파일 등 다양한 파일 형식을 읽는 방법도 있으며, 읽은 내용을 활용하는 다양한 방법들도 존재합니다. 파일을 읽는 작업은 프로그래밍에서 기본 중의 기본이므로 꼭 알고 있어야 합니다.

## 관련 자료:

- [Node.js 파일 시스템 모듈 공식 문서](https://nodejs.org/api/fs.html)
- [Hypertext Transfer Protocol with Javascript (XMLHttpRequest)](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest)
- [Browser-based File Handling](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)