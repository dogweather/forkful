---
title:                "Javascript: 문자열의 길이 찾기"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜 
문자열의 길이를 찾는 것에 참여하는 이유는 단순합니다 - 우리는 인터넷 시대에 살고 있기 때문입니다. 우리는 웹 페이지, 어플리케이션, 소셜 미디어 등에서 많은 텍스트를 보게 되고, 그 텍스트의 길이를 파악하는 데는 중요한 이슈가 됩니다. 

## 어떻게
문자열의 길이를 찾는 것은 Javascript에서 매우 간단하고 쉽습니다. 아래는 예시 코드와 결과값입니다.

```Javascript
let string = "안녕하세요";
console.log(string.length); // 출력 값: 5
```

문자열에는 글자 뿐만 아니라 공백도 포함되기 때문에, 실제로 입력한 글자 수와 다를 수 있습니다. 이것은 단순히 글자 수가 아니라 글자 + 공백의 수를 의미합니다. 예를 들어, 위의 예시에서 "안" 다음에 공백이 있기 때문에 총 3글자로 카운트됩니다.

또한 문자열의 길이를 찾는 또 다른 방법은 `.length` 메소드를 사용하는 것입니다. 아래는 이 방법의 예제 코드와 결과값입니다.

```Javascript
let string = "안녕하세요";
console.log(string.length()); // 출력 값: 5
```

## 깊이 파고들기
문자열의 길이를 찾는 더 깊은 정보를 알고 싶나요? 그렇다면 아래의 몇 가지 사실을 확인해보세요.

- `.length`는 Javascript의 내장된 속성으로 다른 데이터 타입에는 적용되지 않습니다. 즉, 숫자나 논리값에는 적용할 수 없습니다.
- `.length`는 문자열의 길이를 찾는 유일한 방법은 아닙니다. `.size()` 메소드를 사용할 수도 있고, `Array.from()` 메소드로 배열을 만들어 그 길이를 확인할 수도 있습니다.
- 긴 문자열을 잘라서 특정 부분만 확인하려면 `.substring()` 메소드를 사용할 수 있습니다. 예를 들어, `"Hello World".substring(0,5)`는 "Hello"를 반환합니다.

## 참고자료
- [Javascript에서 문자열의 길이를 찾는 방법](https://www.tutorialspoint.com/find-the-length-of-a-string-in-javascript)
- [문자열과 숫자의 차이점이 뭘까요?](https://velog.io/@chalkboard/문자열과-숫자의-차이점이-뭘까요-q9gncwj6gi)
- [`.substring()` 메소드에 대한 자세한 정보](https://www.w3schools.com/jsref/jsref_substring.asp)