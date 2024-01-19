---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

**## 뭐 & 왜?**

임시 파일은 프로그램이 일시적으로 사용하는 데이터를 저장하는 파일입니다. 빠른 응답 시간을 위해 데이터를 검색하거나, 크기가 큰 작업을 처리하거나, 백업을 만들 때 프로그래머들은 임시 파일을 생성합니다.

**## 어떻게:**

자바스크립트에서는 `fs`라는 내장 라이브러리를 사용해서 임시 파일을 생성할 수 있습니다. 

```Javascript 
var fs = require('fs');

fs.open('mynewfile2.txt', 'w', function (err, file) {
  if (err) throw err;
  console.log('Saved!');
});
```

위 코드는 `mynewfile2.txt`라는 새 파일을 생성하고, 파일이 이미 있다면 덮어씁니다.

**## 심화:**

1. **역사적 맥락**: 임시 파일 생성은 컴퓨팅의 초기부터 사용되어 왔습니다. 이는 컴퓨터 메모리의 한계를 극복하고, 데이터 손실을 방지하는 데 중요합니다.

2. **대체 가능성**: 파일 시스템을 사용하지 않고 메모리에서 직접 데이터를 처리하는 인메모리 데이터베이스 같은 다른 방법들도 존재합니다.

3. **구현 세부 사항**: 자바스크립트는 `fs`라이브러리를 통해 파일 시스템에 접근할 수 있게 해줍니다. 노드JS에서 `fs`모듈을 사용하여 파일 작업을 수행합니다.

**## 참조 자료:**

[Node.js fs Module](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)

[Understanding the Node.js fs module](https://nodejs.dev/learn/the-nodejs-fs-module)

[How to Create, Read, Update, and Delete files with Node.js](https://www.digitalocean.com/community/tutorials/how-to-use-the-node-js-fs-module-to-work-with-files-and-directories)