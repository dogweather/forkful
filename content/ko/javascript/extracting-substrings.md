---
title:                "Javascript: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜?

서브스트링(Substring) 추출을 하게 되면 문자열의 일부만을 추출할 수 있습니다. 이는 특정 문자열을 찾거나, 문자열을 잘라서 원하는 데이터를 추출할 수 있어서 많은 사람들이 이 기능을 사용합니다.

## 추출하는 방법은?

자바스크립트를 사용하여 문자열에서 서브스트링을 추출하는 방법은 간단합니다.

```Javascript
// 문자열을 변수에 저장합니다.
let str = "Hello world";

// 서브스트링을 추출합니다.
let substring = str.substring(0, 5);

// 결과를 콘솔에 출력합니다.
console.log(substring);

// Output: "Hello"
```

위의 예시 코드에서는 "Hello world"라는 문자열에서 0번째부터 5번째 글자까지 서브스트링을 추출해 "Hello"라는 결과를 얻게 됩니다. substring() 함수는 시작 인덱스와 끝 인덱스를 인자로 받으며, 해당 인덱스 사이의 문자열을 추출합니다.

또는, 다음과 같이 "..."와 같은 특수 문자를 사용하여도 간단하게 서브스트링을 추출할 수 있습니다.

```Javascript
// 문자열을 변수에 저장합니다.
let str = "Hello world";

// "..." 특수 문자를 사용하여 서브스트링을 추출합니다.
let substring = str.substring(0, str.indexOf("..."));

// 결과를 콘솔에 출력합니다.
console.log(substring);

// Output: "Hello"
```

위의 예시 코드에서는 "Hello world"라는 문자열에서 "..." 특수 문자가 나오기 전까지의 문자열을 추출하여 "Hello"라는 결과를 얻게 됩니다.

## 더 알아보기

서브스트링 추출에 대해서 좀 더 깊이 알아보겠습니다. 자바스크립트에서는 다양한 문자열 메소드를 사용하여 서브스트링을 추출할 수 있습니다.

```Javascript
// 문자열을 변수에 저장합니다.
let str = "Hello world";

// charAt() 메소드를 사용하여 인덱스에 해당하는 문자를 추출합니다.
let char = str.charAt(6);

// 결과를 콘솔에 출력합니다.
console.log(char);

// Output: "w"
```

위의 예시 코드에서는 charAt() 메소드를 사용하여 인덱스 6번째에 해당하는 문자 "w"를 추출하였습니다.

또는, substring() 함수 외에도 substr() 함수를 사용하여 서브스트링을 추출할 수 있습니다.

```Javascript
// 문자열를 변수에 저장합니다.
let str = "Hello world";

// substr() 함수를 사용하여 인덱스 6번째부터 5개의 글자를 추출합니다.
let substring = str.substr(6, 5);

// 결과를 콘솔에 출력합니다.
console.log(substring);

// Output: "world"
```

위의 예시 코드에서는 substr() 함수를 사용하여 인덱스 6번째부터 5개의 글자를 추출하여 "world"라는 결과를 얻게 됩니다.

## 이어서 읽어보세요.

- [자바스크립트 문자열 메소드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String)
- [substring() 함수](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [substr() 함수](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substr)

## 참고 자료

- [Javascript substring() 함수로 문자열 자르기](https://www.w3schools.com/jsref/jsref_substring.asp)
- [Javascript substr() 함수로 문자열 자르기](https://www.w3schools.com/jsref/jsref_substr.asp)
- [JavaScript substring() 함수로 시작 인덱스와 끝 인덱스의 값 지정