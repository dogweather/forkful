---
title:                "Javascript: 부분 문자열 추출하기"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜 substrings 추출하는가?

Substring 추출은 문자열 내에서 특정 부분을 선택해 내는 과정입니다. 이는 특정한 문자열 데이터를 원하는 형식으로 가공할 때 매우 유용합니다. 예를 들어, 영화 제목만 추출하거나 전화번호의 국가 번호를 제외한 나머지 번호를 추출하는 등 다양한 용도로 활용할 수 있습니다.

# 어떻게 하나요?

예를 들어, 아래와 같은 문자열이 있다고 가정해봅시다.

```Javascript
let movieTitle = "어벤져스: 엔드게임";
```

우리는 이 문자열에서 "어벤져스" 부분만 추출해보겠습니다. 이를 위해 `substring()` 메소드를 사용할 수 있습니다.

```Javascript
let sub = movieTitle.substring(0, 6);
console.log(sub); // "어벤져스"
```

위 예시에서 보듯이, `substring()` 메소드를 이용하면 원하는 부분을 추출할 수 있습니다. 첫 번째 파라미터는 시작 위치를, 두 번째 파라미터는 끝 위치를 나타냅니다. 위의 코드에서는 "어벤져스"부터 "엔드게임"까지의 문자열을 추출하기 위해 0과 6이라는 인덱스를 지정했습니다. 또한 끝 위치를 지정하지 않으면 문자열의 끝까지 추출할 수 있습니다.

# 깊게 파헤쳐보기

`substring()` 메소드 외에도 `slice()`와 `substr()` 메소드를 이용해 동일한 작업을 할 수 있습니다. 그러나 이들 메소드는 인덱스를 다르게 지정해주어야 합니다. `slice()` 메소드는 끝 위치를 지정하지 않으면 문자열의 끝까지 추출하는 반면, `substr()` 메소드는 두 번째 파라미터에 추출할 문자열의 개수를 지정합니다.

또한, JavaScript에서는 정규표현식을 이용해 문자열을 추출하는 방법도 제공합니다. `match()` 메소드를 이용해 정규표현식에 해당하는 부분을 추출할 수 있습니다. 이는 `substring()` 메소드와는 달리, 문자열 내의 여러 부분을 한 번에 추출할 수 있다는 장점이 있습니다.

# 또 보기

- [MDN docs - substring()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [MDN docs - slice()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN docs - substr()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substr)
- [MDN docs - match()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/match)