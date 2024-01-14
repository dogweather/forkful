---
title:                "Javascript: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 방법은 프로그래밍에서 중요한 요소입니다. 이를 통해 사용자가 자신이 작성한 데이터를 읽고 처리할 수 있으며, 더 나은 사용자 경험을 제공할 수 있습니다.

## 어떻게

```Javascript
let fs = require('fs'); //노드 파일 시스템 모듈을 불러옴

fs.readFile('sample.txt', 'utf8', function(err, data){ //텍스트 파일을 읽음
    if(err){ //에러 처리
        console.log("에러 발생: " + err);
    } else {
        console.log(data); //파일의 내용을 출력
    }
});
```

위 코드는 노드 파일 시스템 모듈을 사용해 텍스트 파일을 읽는 예시입니다. 여러분도 자신이 사용하는 프로그래밍 언어에서 파일을 읽는 기능을 제공할 것입니다. 이를 통해 사용자 입력이나 자동적으로 생성되는 데이터를 읽어와 활용할 수 있습니다.

## 깊이 파헤치기

파일을 읽는 방법은 다양한 방법으로 구현할 수 있습니다. 위 예시에서는 노드 파일 시스템 모듈을 사용했지만 여러분이 사용하는 다른 프로그래밍 언어에서도 비슷한 기능을 제공할 것입니다. 또한 파일을 읽는 과정에서 발생할 수 있는 에러 처리나 다양한 데이터 처리 방법에 대해서도 함께 공부하게 될 것입니다.

## 다른 자료들

* 파일 열기 및 읽기 in JavaScript: https://www.geeksforgeeks.org/reading-a-file-line-by-line-using-node-js/
* 노드 파일 시스템 모듈: https://nodejs.org/api/fs.html
* 파일 처리와 에러 핸들링 in JavaScript: https://www.w3schools.com/js/js_file_handling.asp