---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

임시 파일을 만드는 것은 프로그램 실행 도중 일시적으로 데이터를 저장하기 위한 중간 공간을 만드는 것입니다. 프로그래머들은 가능한 한 빠른 데이터 처리와 함께 데이터 손실을 예방하려는 목적으로 이를 수행합니다.

## 어떻게 하는가:

Elm은 순수 함수형 언어로서, 일반적인 파일 시스템 작업(임시 파일 생성을 포함)을 직접 수행하는 것은 불가능합니다. 대신 Elm은 서버 측 코드나 JavaScript와 같은 다른 언어를 통해 이를 수행할 수 있습니다. 

예를 들어, 다음과 같이 JavaScript와 Elm을 통합할 수 있습니다.

JavaScript:

```JavaScript
var app = Elm.Main.init();
app.ports.askForTempFile.subscribe(function() {
  var fName = 'temp' + Date.now() + '.txt';
  fs.writeFile(fName, 'Some content', function (err) {
    if (err) throw err;
    app.ports.receiveTempFile.send(fName);
  });
});
```

Elm:

```Elm
port module Main exposing (..)

port askForTempFile : Cmd msg

port receiveTempFile : (String -> msg) -> Sub msg
```

## Deep Dive

임시 파일을 만드는 일은 프로그래밍 역사의 초기부터 있었습니다. 이것은 프로그래머들이 빠르게 데이터를 처리하고, 프로그램의 상태를 저장하고, 제어를 갖기 위한 방법이기도 합니다. Elm과 같은 순수하게 기능적인 언어에서는 함수의 부수효과를 최소화하기 때문에 직접적으로 임시 파일을 만들 기능이 없습니다. 대신, Elm은 자바스크립트와 같은 언어와 함께 작동하여 이를 처리합니다.

대안으로는, Elm의 웹 요청 기능을 사용해서 서버에 파일을 요청하는 방법이 있습니다. 이 경우, 파일은 서버에서 처리되며 Elm은 결과를 수신하게 됩니다.

## 참조자료

1. Elm 가이드 : Elm와 JavaScript의 상호작용에 대한 자세한 정보.(https://guide.elm-lang.org/interop/)
2. MDN 웹 문서 : JavaScript를 이용한 파일의 생성, 읽기, 수정 방법에 대해 배울 수 있는 곳. (https://developer.mozilla.org/ko/docs/Web/API/FileSystem)