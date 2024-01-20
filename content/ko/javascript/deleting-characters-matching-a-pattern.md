---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

문자 패턴을 지우는것은 지정된 문자/문자열 청사진에 맞는 모든 문자를 소스 문자열에서 삭제하는 프로그래밍 기능입니다. 프로그래머들이 이것을 사용하는 주요한 이유는 사용하지 않는 데이터를 최적화하거나 필요 없는 문자를 제거하여 데이터를 정리하기 위해서입니다.

## 어떻게:

Javascript에서, 우리는 정규표현식(RegEx)와 `replace()` 메서드를 함께 사용하여 문자열에서 패턴에 일치하는 문자를 삭제할 수 있습니다.

```Javascript
let myStr = "Hello, Javascript !@# $% ^^";
myStr = myStr.replace(/[^a-zA-Z ]/g, "");
console.log(myStr);
```

위의 코드는 특수문자를 모두 삭제하므로 출력은 `"Hello Javascript "`를 출력하게 됩니다.

## 깊게 살펴보기:

1) 패턴 일치 문자 삭제는 매우 오래된 프로그래밍 개념입니다. 문자열 처리와 데이터 클리닝의 주요 부분입니다.
2) 문자열에서 일치하는 문자를 제거하는 다른 방법으로는 split()와 join() 메서드를 사용하는 것입니다. 이 방법이 replace() 메서드보다 느릴 수 있습니다.
3) `replace()` 함수는 자바스크립트 에서 String 데이터 타입에 내장된 메서드이다. 이 함수는 첫 번째 인자로 주어진 패턴 또는 문자열을 찾아 두 번째 인자로 교체한다. 'g' 플래그는 표현식을 전역적으로 적용할 것임을 의미한다.

## 참고 자료:

1) [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
2) [JavaScript Info: Regular expressions](https://javascript.info/regular-expressions)
3) [W3Schools: JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)
4) [JavaScript Tutorial: JavaScript Regular Expression](https://www.javascripttutorial.net/javascript-regular-expression/)