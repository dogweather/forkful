---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# 문자열 연결하기: 무엇이며 왜 해야하는가?

## 무엇이며 왜?

문자열 연결은 두 개 이상의 문자열을 하나로 결합하는 것입니다. 이 작업을 통해 동적으로 생성된 내용을 포함하는 문자열을 재사용하거나 만들 수 있습니다.

## 어떻게?

```Javascript
let str1 = '안녕';
let str2 = ',';
let str3 = ' 세상아!';
let greeting = str1 + str2 + str3;
console.log(greeting); // "안녕, 세상아!"
```

템플릿 문자열을 사용하면 더 간단하게 할 수 있습니다:

```Javascript
let str1 = '안녕';
let str3 = ' 세상아!';
let greeting = `${str1}, ${str3}`;
console.log(greeting); // "안녕, 세상아!"
```

## 심화 학습

문자열 연결은 오래 전 컴퓨터 과학의 첫날부터 있었습니다. 복잡한 출력을 만들기 위해선 필수적인 기능입니다. 대안으로는 배열 결합, `concat()` 함수, `join()` 함수 등이 있습니다. 

`+`와 `+=`연산자는 문자열을 연결하고 새 문자열을 반환합니다. 반면에 `concat()`과 `join()` 함수는 기존의 문자열을 변경하지 않습니다. 

## 참고 자료 

[MDN 문자열 연결](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/concat)  
[MDN 템플릿 리터럴](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Template_literals)  
[문자열 연결 대 애스팅의 성능 차이](https://www.joelonsoftware.com/2001/12/11/back-to-basics/)