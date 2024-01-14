---
title:                "Javascript: 패턴과 일치하는 문자 삭제하기"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

코드에서 특정 패턴과 일치하는 문자를 삭제하는 작업은 데이터나 텍스트를 정제하거나 필터링하는데 유용합니다.

## 어떻게

```Javascript
// 문자열 변수 생성
let sentence = "Hello, my name is John. I have 3 cats and 2 dogs.";

// 정규식 패턴을 사용하여 모든 숫자를 삭제하는 함수
function deleteNumbers(str) {
    // 정규식 표현 생성
    let pattern = /[0-9]/g;
    // 패턴과 일치하는 문자 제거 후 결과 반환
    return str.replace(pattern, '');
}

// 함수 호출 및 결과 출력
console.log(deleteNumbers(sentence)); // Hello, my name is John. I have  cats and  dogs.
```

위의 코드는 문자열에서 모든 숫자를 삭제하는 간단한 함수의 예시입니다. 정규식 표현을 사용하여 패턴을 정의하고, 문자열 메서드를 활용하여 해당 패턴과 일치하는 문자를 제거하는 방법을 보여줍니다.

## 딥 다이브

정규식은 특정 패턴과 일치하는 문자를 찾는 강력한 도구입니다. 다양한 패턴을 사용하여 긴 문자열에서 원하는 부분만을 추출하거나 제거할 수 있습니다. 예를 들어, 특정 단어가 포함된 모든 문장을 찾거나 HTML 코드에서 태그를 삭제하는 등 다양한 상황에서 유용하게 사용될 수 있습니다.

정규식 패턴을 작성하는 것은 처음엔 복잡해보일 수 있지만, 간단한 패턴부터 시작하여 차근차근 익혀나가면 됩니다.

## 참고 자료

- [정규식 튜토리얼](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [정규식 패턴 생성기](https://regexr.com/)
- [정규식을 활용한 문자열 검색 및 변경](https://velog.io/@cyberworlds/JS-정규식을-활용한-문자열-검색-및-변경)