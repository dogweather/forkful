---
title:    "Javascript: 텍스트 파일 작성하기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 왜?

텍스트 파일을 작성하는 것은 프로그래머가 프로그램을 작성할 때 중요한 부분입니다. 이는 코드를 구성하고 보관하기 위해 필요한 정보를 담고 있는 파일이며, 나중에 코드를 다시 사용하거나 다른 사람과 공유할 수 있도록 합니다.

## 방법

먼저, 텍스트 파일을 작성하기 위해 우리는 Javascript XMLHttpRequest 객체를 사용할 것입니다. 이 객체는 서버로부터 데이터를 가져올 수 있도록 해주는 기능을 제공합니다. 아래는 간단한 예제 코드입니다.

```Javascript
// XMLHttpRequest 객체 생성
var xhttp = new XMLHttpRequest();
// 파일을 열기 위한 GET 요청 보내기
xhttp.open("GET", "textfile.txt", true);
// 파일을 불러오기
xhttp.onreadystatechange = function() {
    // 서버로부터 응답이 오면 데이터를 출력하기
    if (this.readyState == 4 && this.status == 200) {
        // 파일에서 읽은 데이터를 변수에 저장
        var data = this.responseText;
        // 데이터 출력
        console.log(data);
    }
};
// 요청 보내기
xhttp.send();
```

위의 예제 코드에서는 XMLHttpRequest 객체를 생성하고, GET 요청을 보내서 파일을 불러온 뒤, 서버로부터 응답이 오면 데이터를 출력하는 방식으로 작성됩니다. 이것은 파일을 읽기 위해 필요한 가장 기본적인 방법입니다.

## 깊이 파고들기

텍스트 파일을 작성하는 것은 간단한 작업처럼 보일 수 있지만, 더 깊이 들어가보면 더 복잡한 작업이 될 수 있습니다. 예를 들어, 우리는 파일을 읽기 전에 먼저 파일의 존재 여부를 확인해야 합니다. 또는 파일을 작성하는 도중에 오류가 발생했을 때 이를 처리하는 방법도 생각해야 합니다. 이런 작업을 위해 사용할 수 있는 여러 가지 방법이 있기 때문에 깊게 공부하고 습득하는 것이 중요합니다.

## 참고 자료

- [MDN: XMLHttpRequest](https://developer.mozilla.org/ko/docs/Web/API/XMLHttpRequest)
- [W3Schools: Ajax - Load the JSON data from a file](https://www.w3schools.com/js/js_ajax_intro.asp)
- [TechSight: How to read and write files in JavaScript](https://www.techsight.nl/programming/javascript/read-and-write-files-with-javascript/)
- [Tutorials Teacher: JavaScript File Handling](https://www.tutorialsteacher.com/javascript/javascript-file-handling)