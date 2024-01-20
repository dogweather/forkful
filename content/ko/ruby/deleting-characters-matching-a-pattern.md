---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜 & 왜 사용하나요?
문자열에서 패턴에 일치하는 문자를 삭제하는 것은 프로그래밍에서 흔히 볼 수 있는 작업입니다. 이 작업은 불필요한 공백을 제거하거나, 특정 문자를 포함하지 않은 깔끔한 문자열을 원할 때 유용합니다.

## 코드 예제:
Ruby에서는 `delete` 메소드를 이용해 문자열에서 일치하는 문자를 삭제할 수 있습니다. 

```Ruby
str = "Hello, World!"
clean_str = str.delete(',')
print clean_str
```

이 코드는 `"Hello, World!"`에서 쉼표를 제거하고 출력합니다. 결과는 `"Hello World!"`입니다.

## 깊게 알아보기
- 역사적 맥락: 삭제 메소드는 Ruby가 시작된 1990년대 중반부터 있었습니다. 이 메소드는 오늘날 우리가 복잡한 문자열 조작 작업을 쉽게 처리할 수 있도록 합니다.
- 대체 방법: `gsub` 메소드를 사용하여 일치하는 문자를 삭제할 수도 있습니다. `gsub`은 일치하는 패턴을 다른 문자열로 바꾸는 데 사용됩니다. 삭제하려면 빈 문자열("")로 바꾸면 됩니다.
```Ruby
str = "Hello, World!"
clean_str = str.gsub(',', '')
print clean_str
```
- 실행 세부사항: `delete` 메소드는 문자열을 훑어 일치하는 각 문자를 찾아 삭제합니다. 이는 빠른 작업이지만, 삭제할 문자가 많거나 문자열이 매우 길다면 시간이 걸릴 수 있습니다.

## 추가 자료
해당 주제에 대한 추가 공부를 위해 아래 링크를 참조하십시오.
- [Ruby Doc for String#delete](https://ruby-doc.org/core-3.0.0/String.html#method-i-delete)
- [Ruby Doc for String#gsub](https://ruby-doc.org/core-2.6/String.html#method-i-gsub)