---
title:                "디버그 출력하기"
html_title:           "Gleam: 디버그 출력하기"
simple_title:         "디버그 출력하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜
딱딱한 코딩 작업을 하다보면 디버그 출력을 사용하게 됩니다. 이는 오류를 찾는 데 매우 유용합니다.

## 어떻게
코딩을 할 때 디버그를 위해 출력을 사용하는 방법은 간단합니다. 먼저, ``` Gleam log ``` 코드 블록을 사용하여 디버그 메시지를 출력합니다.

```Gleam
log("이것은 디버그 메시지입니다.")
```

위 코드를 실행하면 콘솔에 "이것은 디버그 메시지입니다."라는 메시지가 출력됩니다. 이제 여러분은 코딩 중 발생한 오류를 쉽게 파악할 수 있습니다.

## 깊게 파악하기
디버그 출력은 오류를 찾는 데 있어서 매우 유용하지만, 많은 사람들이 잊거나 무시하는 기능입니다. 하지만 잘 사용한다면 코딩 과정에서 매우 편리하게 사용할 수 있습니다.

디버그 출력을 사용하면 어떤 변수가 어떤 값을 가지고 있는지, 어떤 코드가 실행되었는지 등을 확인할 수 있습니다. 또한 디버그 메시지를 추가하여 원인을 빠르게 찾을 수 있습니다.

## 더 알아보기

디버그 출력은 코딩 작업을 하는 데 있어서 필수적이지만, 잘 사용하지 않는 경우가 많습니다. 따라서 자세한 내용을 알아보시려면 아래 링크를 참고해주세요.

## 관련 링크
- [Gleam 공식 문서](https://gleam.run/)
- [디버그 출력 기능 설명](https://gleam.run/017-debugging.html)