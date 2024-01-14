---
title:                "TypeScript: 문자열의 길이 찾기"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 왜
우리 모두는 문자열을 자주 다루게 됩니다. 때로는 사용자 입력 값을 검증하고, 때로는 데이터를 처리하고, 때로는 출력을 포맷팅 하는 등 다양한 이유로 문자열을 사용합니다. 그렇기 때문에 문자열의 길이를 알아야 할 필요가 있습니다. 이 글에서는 TypeScript로 문자열의 길이를 찾는 방법을 소개하겠습니다.

## 방법
우선 문자열의 길이를 찾는 가장 간단한 방법은 내장 함수 `length`를 사용하는 것입니다. 아래의 예시 코드를 보면 이해하기 쉽습니다.

```TypeScript
let str: string = "안녕하세요";
console.log(str.length); // 출력 결과: 5
```

위 코드에서 우리는 `str` 변수에 "안녕하세요"라는 문자열을 할당하고, 이 문자열의 길이를 `length` 함수를 사용하여 찾습니다. 출력 결과는 5가 나오는 것을 볼 수 있습니다. 여기서 잠깐, 한글의 경우에는 그 글자 하나가 두 개의 바이트로 이루어지기 때문에 인식되는 문자열의 길이가 다를 수 있습니다. 하지만 우리가 흔히 사용하는 영어나 숫자, 기호 등은 모두 한 개의 바이트로 이루어지기 때문에 그 길이를 정확하게 알 수 있습니다.

또 다른 방법으로는 `split` 함수를 사용하는 방법이 있습니다. 이 함수는 문자열을 특정 기준으로 분리하여 배열로 만들어 주는 함수입니다. 예를 들어, 마침표를 기준으로 문자열을 분리하면 마침표의 개수와 같은 길이를 구할 수 있습니다. 아래 코드를 보면 더 명확하게 이해할 수 있습니다.

```TypeScript
let sentence: string = "안녕하세요. 제 이름은 TypeScript입니다.";
let words: string[] = sentence.split(".");
console.log(words.length); // 출력 결과: 2
```

위 코드에서는 `sentence` 변수에 한글과 영어가 섞인 문자열을 할당하고, `split()` 함수를 사용하여 마침표를 기준으로 분리한 뒤 그 길이를 찾는 방법을 보여주고 있습니다. 출력 결과는 마침표의 개수인 2가 나오는 것을 확인할 수 있습니다.

## 딥 다이브
문자열의 길이를 찾는 방법은 위에서 소개한 것 외에도 더 많이 존재합니다. 가령, 정규표현식을 사용하여 특정 문자열의 개수를 세는 방법이나, 반복문을 활용하여 문자열의 한 글자씩을 체크하며 개수를 세는 방법 등이 있습니다. 하지만 이런 방식들은 보통 문자열의 길이를 찾는 데에 있어서 비효율적일 수 있기 때문에, 위에서 소개한 내장 함수나 `split` 함수를 사용하는 것이 효율적입니다. 더 자세한 내용은 관련 링크를 참고하시면 좋을 것 같습니다.

## 봐봐
[MDN 문자열 길이](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/length) - MDN에서 제공하는 문자열 길이에 대한 설명

[정규표현식을 활용한 문자열 길이 찾기](https://programmingwithmosh.com/javascript/javascript-string-length/) - 정규표현식을 사용하여 문자열 길이를 찾는 방법에 대한 자세한 설명

[TypeScript