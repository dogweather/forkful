---
title:    "Javascript: 텍스트 검색 및 교체"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜: 
문자열을 검색하고 바꾸는 과정은 다양한 프로그래밍 작업을 수행하는 데에 필수적입니다. 문자열을 조작하고 텍스트를 효율적으로 대체하는 것은 웹 개발을 비롯한 여러 분야에서 매우 유용합니다.

## 사용 방법:
```Javascript
// 주어진 문자열에서 특정 단어를 찾고 다른 단어로 바꾸는 예시
var str = "안녕하세요, 저는 Javascript를 사용하는 개발자입니다.";
var newStr = str.replace("개발자", "프로그래머");
console.log(newStr); // "안녕하세요, 저는 Javascript를 사용하는 프로그래머입니다."
```

```Javascript
// 정규식을 사용하여 문자열 대체하기
var str = "오늘은 날씨가 좋습니다. 내일은 비가 올 수도 있습니다.";
var newStr = str.replace(/오늘|내일/g, "다음"); // 모든 "오늘"과 "내일"을 "다음"으로 바꿈
console.log(newStr); // "다음은 날씨가 좋습니다. 다음은 비가 올 수도 있습니다."
```

## 심층 분석:
문자열을 검색하고 바꾸는 메서드는 문자열 객체의 내장 함수 중 하나인 `replace()`를 사용합니다. 첫 번째 인자로는 찾고자 하는 문자열 또는 정규식을, 두 번째 인자로는 바꾸고자 하는 문자열을 넣어줍니다. 그리고 `replace()` 메서드는 새로운 문자열을 반환하며, 원본 문자열은 변경되지 않습니다.

정규식을 사용하여 문자열을 검색하고 대체하는 것은 더욱 강력한 기능을 제공합니다. 위의 예시에서는 `/오늘|내일/g`라는 정규식을 사용하여 모든 "오늘"과 "내일"을 동시에 대체하였습니다.

## 관련 링크:
- [MDN Web Docs: `replace()` 메서드 문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [정규식 문법](https://regexr.com/)