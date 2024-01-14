---
title:    "Javascript: 서브스트링 추출하기"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜: JavaScript에서 부분 문자열 추출을 하는 이유

자바스크립트에서 부분 문자열을 추출하는 것은 문자열 처리에 유용한 기술입니다. 대부분의 프로그래밍 언어에서 제공하는 내장 함수를 사용하지 않고도 세밀한 조작이 가능하며, 유용한 유니코드 기능을 활용할 수 있습니다.

## 방법: 코드 블록과 예시 출력을 활용한 부분 문자열 추출

우선 다음과 같이 문자열을 선언합니다.

```Javascript
let str = "안녕하세요! 반가워요";
```

### substring() 메소드를 이용한 일부 문자열 추출

JavaScript에서는 substring() 메소드를 이용하여 일부 문자열을 추출할 수 있습니다. 이 메소드는 첫 번째 인자로 시작 위치, 두 번째 인자로 추출할 문자의 길이를 받습니다. 아래는 위에서 선언한 str 변수에서 시작 위치가 0이고 길이가 5인 문자열을 추출하는 예시 코드입니다.

```Javascript
str.substring(0,5);  // 출력 값: "안녕하세요"
```

### slice() 메소드를 이용한 일부 문자열 추출

slice() 메소드의 경우 첫 번째 인자로 시작 위치, 두 번째 인자로 추출할 문자의 끝 위치를 받습니다. 따라서 아래 코드는 str 변수에서 2번째부터 5번째까지의 문자열을 추출합니다.

```Javascript
str.slice(2,6);  // 출력 값: "녕하세"
```

### substr() 메소드를 이용한 일부 문자열 추출

substr() 메소드의 경우 첫 번째 인자로 시작 위치, 두 번째 인자로 추출할 문자의 길이를 받습니다. 아래 코드는 str 변수에서 시작 위치가 7이고 길이가 3인 문자열을 추출합니다.

```Javascript
str.substr(7,3);  // 출력 값: "반가워"
```

## 깊은 들여다보기: 부분 문자열 추출에 대한 추가 정보

위에서 소개한 메소드 외에도 JavaScript에서는 정규표현식을 이용하여 부분 문자열을 추출하는 방법이 있습니다. 또한 유니코드를 포함한 여러 언어의 문자열을 다룰 수 있는 유용한 기능들이 많이 존재합니다.

# 더 알아보기

[MDN 문서 - substring() 메소드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substring)

[MDN 문서 - slice() 메소드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/slice)

[MDN 문서 - substr() 메소드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/substr)

[정규표현식을 이용한 문자열 추출 예제](http://www.nextree.co.kr/p4327/)

[JavaScript로 유니코드 문자열 다루기](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Internationalization#Basic_usage_and_examples)