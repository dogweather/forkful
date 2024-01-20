---
title:                "텍스트 파일 작성하기"
html_title:           "Javascript: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 무엇인가요? & 왜 해요?

텍스트 파일을 쓰는 것은 프로그래머들이 코드를 저장하고 공유하기 위해 사용하는 일반적인 방법입니다. 이는 사용자가 읽기 쉽고 이해하기 쉬운 형식으로 정보를 저장할 수 있도록 도와줍니다. 또한 텍스트 파일은 다양한 운영 체제에서 호환성이 높으며, 텍스트 파일을 이용하면 간단하게 백업하고 파일을 전송할 수 있습니다.

# 어떻게 하나요?

```Javascript
const fs = require('fs');

// 새로운 텍스트 파일 생성
fs.writeFile('example.txt', '안녕하세요!', (err) => {
  if (err) throw err;
  console.log('새로운 텍스트 파일이 생성되었습니다.');
});

// 기존 파일에 내용 추가
fs.appendFile('example.txt', ' 잘 지내시죠?', (err) => {
  if (err) throw err;
  console.log('파일에 내용이 추가되었습니다.');
});

// 파일 내용 읽어오기
fs.readFile('example.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

출력:

```
새로운 텍스트 파일이 생성되었습니다.
파일에 내용이 추가되었습니다.
안녕하세요! 잘 지내시죠?
```

# 깊이 들여다보기

1. 역사적 배경: 텍스트 파일은 오래된 형식으로서, 컴퓨터의 데이터 저장 방식 중 하나입니다. 현재에도 여전히 사용되는 이유 중 하나는 단순함과 호환성 때문입니다.

2. 대안: 텍스트 파일 외에도 프로그래머들은 다양한 형식의 파일을 사용할 수 있습니다. 예를 들어, JSON, CSV, XML 등의 파일 형식이 있습니다. 이러한 형식은 데이터를 더 구조화하여 저장할 수 있지만, 일반 사용자에게는 읽기와 이해가 더 어려울 수 있습니다.

3. 구현 세부 사항: 위의 예제에서 사용한 `fs` 모듈은 Node.js의 내장 모듈로 파일 시스템에 접근할 수 있는 기능을 제공합니다. 다른 프로그래밍 언어에서도 파일을 읽고 쓰는 기능을 지원하는 여러 라이브러리나 모듈이 있습니다.

# 참고 자료

- [Node.js 파일 시스템 모듈 공식 문서](https://nodejs.org/api/fs.html)
- [W3Schools - JavaScript Files 모듈](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)