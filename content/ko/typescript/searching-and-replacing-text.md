---
title:    "TypeScript: 텍스트 검색 및 교체"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 바꾸는 작업에서 이점을 얻을 수 있기 때문에 왜 이 작업을 수행하는지 알아보겠습니다. 텍스트 검색 및 교체는 반복적이고 반복적인 작업을 자동화하는 데 도움이 되므로 개발자의 생산성을 향상시키는 데 도움이 됩니다. 또한 텍스트 검색 및 교체는 오류를 찾고 수정하는 데 유용하며, 대규모 코드베이스에 대한 수정 작업을 빠르고 쉽게 수행할 수 있게 해줍니다.

## 하우투

먼저, `replace()` 메소드를 사용하여 간단한 문자열 검색과 교체를 수행하는 방법을 알아보겠습니다.

```TypeScript
const str = "안녕하세요, TypeScript 학습 중입니다.";
const newStr = str.replace("TypeScript", "JavaScript");

console.log(newStr);
// 출력: 안녕하세요, JavaScript 학습 중입니다.
```

문자열에 대한 정규식을 사용하여 더 복잡한 검색 및 교체 작업을 수행할 수도 있습니다.

```TypeScript
const str = "Posts?posts=123&sort=popular";
const newStr = str.replace(/posts/gi, "articles");

console.log(newStr);
// 출력: articles?articles=123&sort=popular
```

다음은 `replaceAll()` 메소드를 사용하여 모든 일치하는 문자열을 한 번에 교체하는 방법입니다.

```TypeScript
const str = "apple, apple, apple";
const newStr = str.replaceAll("apple", "orange");

console.log(newStr);
// 출력: orange, orange, orange
```

## 딥 다이브

텍스트 검색 및 교체 작업을 보다 정교하게 제어하고 싶다면 정규식을 배우는 것이 좋습니다. 정규식은 특정 문자열 패턴을 찾고 교체하는 데 사용되는 표현식입니다.

예를 들어, 다음과 같은 문자열이 있다고 가정해봅시다.

```
I'm learning TypeScript! It's super fun!
```

만약 `super` 대신 `extremely`으로 바꾸고 싶다면 정규식을 사용하여 다음과 같이 작성할 수 있습니다.

```TypeScript
const str = "I'm learning TypeScript! It's super fun!";
const newStr = str.replace(/super/gi, "extremely");

console.log(newStr);
// 출력: I'm learning TypeScript! It's extremely fun!
```

위 예시에서는 정규식의 `g`와 `i` 플래그를 사용했습니다. `g`는 전역 검색을 의미하며, `i`는 대소문자를 구분하지 않는 검색을 의미합니다.

## 참고 자료

- [JavaScript replace() 메소드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [정규식 소개](https://proglib.io/p/regex/)
- [정규식으로 문자열 검색 및 교체하기](https://www.digitalocean.com/community/tutorials/how-to-use-regex-to-search-for-match-patterns-in-strings-in-javascript-ko)