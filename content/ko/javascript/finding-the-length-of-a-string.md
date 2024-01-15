---
title:                "문자열의 길이 찾기"
html_title:           "Javascript: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 왜

문자열의 길이를 찾는 것은 자바스크립트 프로그래밍에서 유용한 기술입니다. 이를 통해 웹 개발자들은 사용자로부터 입력받은 정보를 처리하고, 특정한 글자 수의 제한을 두는 등 다양한 방식으로 활용할 수 있습니다.

# 코드로 알아보는 방법

```Javascript
// "string"이라는 변수에 문자열을 저장합니다.
let string = "Hello World";

// "length"라는 변수에 문자열의 길이를 저장합니다.
let length = string.length;

// "length"라는 변수에 저장된 문자열의 길이를 출력합니다.
console.log(length); // 11

// "string" 변수에 다른 문자열을 저장하고 위의 과정을 반복할 수 있습니다.
```

위의 예시 코드에서 볼 수 있듯이, 문자열의 길이를 찾는 방법은 간단합니다. 우선, 어떤 문자열을 변수에 저장한 다음, `.length` 메소드를 이용해 그 길이를 가져와서 다른 변수에 저장하고 출력하면 됩니다. 문자열의 길이를 찾는 방법은 변수에 저장하는 것만 다른 것이고, 메소드를 사용하는 부분은 동일합니다. 따라서 변수에 다른 문자열을 저장하고 위의 과정을 반복하면 얼마든지 길이를 찾을 수 있습니다.

# 깊게 파고들어보기

이 방법으로는 문자열의 길이를 확인하는 것만으로는 부족합니다. 우리는 알파벳이나 한글 등 다양한 언어를 다루기 때문입니다. 그렇기 때문에 자바스크립트에서 문자열의 실제 길이를 정확하게 파악하기 위해선 다른 방법이 필요합니다.

자바스크립트에서는 `Unicode`라는 문자 체계를 사용하고 있습니다. 이 체계에서는 문자 하나를 나타내기 위해 보통 2바이트를 사용합니다. 하지만 일부 언어의 문자는 2바이트 이상이 필요할 수 있습니다. 따라서 문자열의 길이를 정확하게 파악하기 위해서는 `Unicode`를 이용해 이중 바이트 문자의 개수를 세어야 합니다.

이를 위해 자바스크립트에서는 `.length` 메소드 대신 `.charCodeAt(index)` 메소드를 사용할 수 있습니다. 이 메소드는 해당 인덱스의 문자에 해당하는 `Unicode` 값을 반환합니다. 이를 이용해 직접 문자열의 각 문자를 탐색하고 얼마만큼의 바이트를 차지하는지 확인하여 문자열의 길이를 정확하게 파악할 수 있습니다.

# 더 알아보기

- [MDN 문서: 문자열 길이 재기](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [MDN 문서: 문자열 길이 파악하기](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [Unicode 정보](https://unicode.org/)
- [Unicode 문자 대표 문자 집합 (UCS)](https://ko.wikipedia.org/wiki/Unicode_%EB%AC%B8%EC%9E%90_%EB%8C%80%ED%91%9C_%EB%AC%B8%EC%9E%90_%EC%A7%91%ED%95%A9_(UCS))

# 참고자료

- [Unicode 문자 집합을 사용한 문자열 길이 계산 예시](https://gist.github.com/mathiasbynens/1010324)
- [자바스크립트 문자열 처리 관련 기술 블로그](https://