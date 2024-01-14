---
title:                "Ruby: 패턴과 일치하는 문자 삭제하기"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜
문자열에서 패턴과 일치하는 문자를 삭제하는 것의 이유는 다양합니다. 일반적으로, 데이터 정제나 특정 문자열 형식을 유지하기 위해서 입니다.

## 사용하는 방법
다음은 몇 가지 예시 코드와 그에 따른 출력입니다.

```Ruby
"Hello, Ruby!".gsub(/[aeiou]/, "") # Output: "Hll, Rby!"

"12345".gsub(/[0-9]/, "") # Output: ""

"Happy birthday!".gsub(/[^aeiou]/, "") # Output: "a i a!"
```

## 깊게 파헤치기
패턴 매칭을 통해 문자를 삭제하는 것은 정규 표현식을 사용한다는 점에서 실제로는 문자열을 다룰 때 매우 유용합니다. 정규 표현식을 더 자세히 알아보기 위해서는 다음의 레퍼런스를 참고해보세요.

## See Also
- [정규표현식을 활용한 문자열 다루기](https://rubykr.github.io/rue/regx/regular_expression.html)
- [루비의 문자열 다루기](https://rubykr.github.io/rue/string.html)
- [특수한 문자만 남기고 싶을때 정규표현식으로 탐색하기](http://www.nextree.co.kr/p4328/)