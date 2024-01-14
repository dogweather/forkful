---
title:                "Javascript: 텍스트 파일 읽기"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 것이 왜 유용한지 궁금하신가요? 자바스크립트로 텍스트 파일을 읽을 수 있다면, 다른 프로그램에서 생성된 데이터를 손쉽게 가져올 수 있기 때문입니다. 예를 들어, 크롤링한 데이터나 다양한 포맷의 파일 등을 읽어서 우리가 원하는 방식으로 가공하고 활용할 수 있습니다.

## 방법

텍스트 파일을 읽는 방법은 간단합니다. 아래의 예시 코드를 참고해보세요.

```Javascript
var fs = require("fs");
var data = fs.readFileSync("sample.txt", "utf8");
console.log(data);
```

위 코드는 `fs` 모듈을 사용하여 `sample.txt` 파일을 읽어온 다음 그 내용을 `data` 변수에 저장하고 콘솔에 출력하는 예시입니다.

## 깊게 알아보기

텍스트 파일을 읽을 때 주의해야 할 점이 있습니다. 파일을 읽을 때 인코딩 타입을 지정해주지 않으면 기본적으로 운영체제에 따라 다른 인코딩 타입을 사용합니다. 이로 인해 파일을 제대로 읽지 못하는 경우가 발생할 수 있으니 주의하셔야 합니다. 또한 파일을 한 번에 모두 읽어오는 것이 아니라 한번에 조금씩 읽어오는 방법도 있으니 필요에 따라 선택해 사용할 수 있습니다.

## 면밀히 살펴보기

더 많은 정보가 필요하시다면 아래 링크들을 참고하시면 됩니다.

[Node.js File System 모듈 레퍼런스](https://nodejs.org/api/fs.html) <br>
[Node.js Buffer 클래스 레퍼런스](https://nodejs.org/api/buffer.html) <br>
[FS 모듈의 readFile() 메소드를 사용한 파일 읽기 방법](https://nodejs.org/docs/latest-v8.x/api/fs.html#fs_fs_readfile_path_options_callback) <br>
[눈물의 Node.js 디버깅 - ES 모듈 읽기](http://meetup.toast.com/posts/188) <br>
[린네랩 - 주니어 웹 개발자의 Node.js 입문(3) 파일과 디렉토리](https://www.slideshare.net/drakejin/nodejs-3-28539574)

## 같이 보기

위에서 말씀드린 `fs` 모듈 외에도 다양한 모듈을 사용하여 파일을 읽고 쓸 수 있습니다. 관련된 다른 모듈들도 함께 살펴보며 더 많은 방법을 익혀보시길 바랍니다.

[fs-extra 모듈 - 기본 `fs` 모듈보다 편리한 파일 관리](https://github.com/jprichardson/node-fs-extra) <br>
[Promise를 조금 더 쉽게 사용할 수 있는 프로미스 래퍼 모듈 - bluebird](http://bluebirdjs.com) <br>
[get-stdin 모듈을 사용하여 stdin으로부터 데이터를 쉽게 받아올 수 있습니다.](https://github.com/sindresorhus/get-stdin)