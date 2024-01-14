---
title:                "TypeScript: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 작성하는 것은 프로그래머가 기존의 코드를 쉽게 유지하고 관리할 수 있도록 만들어줍니다.

## 어떻게

```TypeScript
// 새로운 텍스트 파일 생성 및 쓰기
fs.writeFile("new_file.txt", "This is a new text file!", (err) => {
  if (err) {
    console.log(err);
  }
  console.log("New file created and content is written!");
});

// 기존 텍스트 파일에 추가하기
fs.appendFile("existing_file.txt", "This text is added to an existing file.", (err) => {
  if (err) {
    console.log(err);
  }
  console.log("Content has been appended to existing file!");
});

// 텍스트 파일 읽기
fs.readFile("text_file.txt", (err, data) => {
  if (err) {
    console.log(err);
  }
  console.log(data.toString());
});

```

출력:

```
New file created and content is written!
Content has been appended to existing file!
This is the content of the text file.
```

## 딥 다이브

텍스트 파일을 작성하는 것은 기술적으로 매우 중요한 일입니다. 파일 시스템, 파일 I/O 및 파일 포맷 등과 같은 여러 개념을 이해하고 응용해야 합니다. 또한 적절한 에러 처리와 보안 요소도 고려해야 합니다.

## 또 다른 소재

- [Node.js의 fs 모듈](https://nodejs.org/api/fs.html)
- [TypeScript 문서](https://www.typescriptlang.org/docs/home.html)