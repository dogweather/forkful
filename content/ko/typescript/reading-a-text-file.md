---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

텍스트 파일 읽기는 그 이름에서 알 수 있듯이, 프로그램이 텍스트 파일의 내용을 읽어들이는 것을 말합니다. 이것은 프로그래머들이 텍스트 형식의 데이터를 처리하거나, 사용자가 제공하는 설정 파일을 읽어들일 필요가 있을 때 주로 사용됩니다.

## 어떻게:

텍스트 파일 읽기를 TypeScript에서 하기 위해, Node.js의 file system 모듈을 사용하면 됩니다.

```TypeScript
import fs from "fs";

fs.readFile('textfile.txt', 'utf8', function(err, data){
    if(err) throw err;
    console.log(data);
});
```

이 코드는 'textfile.txt'라는 이름의 파일을 읽습니다.. 출력은 파일의 내용입니다.

## Deep Dive

텍스트 파일을 읽어오는 것은 프로그래밍의 초창기부터 있었던 기능 중 하나입니다. 예전에는 저수준 언어로 직접 파일 시스템을 다루는 코드를 작성했지만, 이제는 대부분의 언어에서 표준 라이브러리 혹은 내장 함수 를 통해 쉽게 파일을 읽어올 수 있게 되었습니다.

앞에서 언급한 fs 모듈 외에도, readline 모듈로 텍스트 파일의 라인을 개별적으로 읽어오는 것도 가능합니다. 또한, 스트림을 사용해서 큰 파일을 효과적으로 처리할 수도 있습니다.

구현 세부사항에 대해 말하자면, Node.js에서 제공하는 이 모듈들은 내부적으로 C++로 작성된 libuv 라이브러리를 사용해서 비동기 IO를 지원합니다. 이로 인해 파일 읽기가 블로킹이 아닌 논블로킹으로 처리됩니다.

## 참고 자료

* [Node.js fs Documentation](https://nodejs.org/api/fs.html)
* [Node.js readline Documentation](https://nodejs.org/api/readline.html)
* [Node.js Streams](https://nodejs.org/api/stream.html)
* [libuv Documentation](http://docs.libuv.org/)