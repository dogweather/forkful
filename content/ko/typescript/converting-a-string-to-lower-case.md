---
title:                "TypeScript: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는데 참여하려는 이유는 여러가지가 있을 수 있습니다. 예를 들면, 사용자의 입력을 모두 소문자로 변환하여 일관성 있는 데이터를 유지하거나, 문자열 비교를 위해 일관된 형식으로 문자열을 조작해야 할 때가 있을 수 있습니다.

## 하는 방법

문자열을 소문자로 변환하는 방법에 대해 알아보겠습니다. 먼저, `toLowerCase()` 함수를 사용하여 문자열을 소문자로 변환할 수 있습니다. 예제 코드는 아래와 같습니다.

```TypeScript
let str = "HELLO WORLD";
str = str.toLowerCase();

// Output: hello world
```

하지만 이 방법은 문자열을 새로운 변수에 할당하여 변환해야 하기 때문에 원본 문자열이 바뀌지 않습니다. 원본 문자열을 바로 변환하는 방법은 `toLocaleLowerCase()` 함수를 사용하는 것입니다. 예제 코드는 아래와 같습니다.

```TypeScript
let str = "HELLO WORLD";
str.toLocaleLowerCase();

// Output: hello world
```

만약 여러 개의 단어로 이루어진 문자열을 소문자로 변환하고 싶다면, `split()` 함수로 단어를 각각 분리한 후, `map()` 함수와 `toLowerCase()` 함수를 사용하여 각 단어를 소문자로 변환한 뒤 다시 `join()` 함수로 합쳐주면 됩니다. 예제 코드는 아래와 같습니다.

```TypeScript
let str = "HELLO WORLD";
str = str.split(" ").map(word => word.toLowerCase()).join(" ");

// Output: hello world
```

## 깊게 들어가보기

문자열을 소문자로 변환하는 과정에서 가장 중요한 것은 유니코드(Unicode)를 고려하는 것입니다. 유니코드는 전 세계의 모든 문자를 컴퓨터에서 사용할 수 있도록 코드로 표현해 놓은 국제 표준입니다. 예를 들어, 영어 알파벳 대문자 `A`는 유니코드로 `U+0041`로 표현되고, 소문자 `a`는 유니코드로 `U+0061`로 표현됩니다. 이처럼 대소문자 간에도 다른 유니코드를 갖기 때문에 문자열을 소문자로 변환할 때, 단순히 알파벳만 생각하는 것이 아니라 유니코드를 고려하여 변환해야 합니다.

또한, `toLowerCase()` 함수는 주어진 문자열을 완전히 소문자로 변환하는 것이 아니라 유니코드 중 대응되는 소문자를 찾아 변환해줍니다. 예를 들어, 터키어의 `I`는 대문자로 `I`와 소문자로 `ı`가 모두 있지만, 영어와 같은 소문자 `i`는 없습니다. 이런 경우, `toLowerCase()` 함수는 대문자 `I`를 소문자 `i`가 아닌 `ı`로 변환해줍니다.

## 또 다른 정보들

이외에도 `toLocaleLowerCase()` 함수는 터키어 방식으로 문자열을 변환할 수 있도록 인자로 `'tr'`을 받아 변환이 가능합니다. 또한, 유니코드에는 `toUpperCase()` 함수와 마찬가지로 대소문자 간에 부등호가 성립하지 않는 다양한 문자들이 있으므로 문자열을 비교할 때에도 유니코드를 고려해서 조작해야