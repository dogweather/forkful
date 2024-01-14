---
title:                "Ruby: 서브스트링 추출하기"
simple_title:         "서브스트링 추출하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜
우리는 때때로 문자열에서 특정 부분만 추출해야 할 때가 있습니다. 이를 통해 문자열을 더 효율적으로 다룰 수 있고 원하는 정보만을 추출할 수 있습니다. 이번 블로그 포스트에서는 루비에서 문자열 추출하는 방법에 대해서 알아보겠습니다.

## 어떻게
우선, `slice` 메서드를 사용하여 문자열에서 원하는 부분을 추출할 수 있습니다. 예를 들어, 다음과 같은 문자열이 있다고 가정해봅시다.
```Ruby
str = "안녕하세요! 루비를 공부하고 있습니다."
```

`slice` 메서드를 사용하여 공부하고 있는 부분만 추출해보겠습니다.
```Ruby
str.slice(6..-1)
```
출력은 다음과 같이 나옵니다.
```Ruby
"루비를 공부하고 있습니다."
```

또 다른 방법으로는 `[]`를 사용하는 것입니다. 위의 예제를 `[]`로 바꿔보면 다음과 같습니다.
```Ruby
str[6..-1]
```
출력은 동일하게 나옵니다.
```Ruby
"루비를 공부하고 있습니다."
```

위의 예제에서는 문자열의 일부를 추출했지만, `[]`나 `slice`를 사용할 때 문자열 내 특정 위치의 문자를 추출하는 것도 가능합니다. 다음 예제를 확인해보세요.
```Ruby
str[0] # 첫 번째 문자 "안"
str[-1] # 마지막 문자 "니다"
```

## 더 깊게 들어가기
루비에서는 문자열을 다루는 데에 유용한 메서드들이 많이 있습니다. 조금 더 자세하게 알아보기 위해 다음 링크들을 참고해보세요.

### String 클래스 메서드: 
https://ruby-doc.org/core-2.6.3/String.html

### 문자열 슬라이싱에 대한 다른 예제들:
https://www.tutorialspoint.com/ruby/ruby_strings.htm

## 또 다른 참고 자료
[Ruby 문자열 함수](https://velog.io/@taehyoungjo/Ruby-문자열-함수)

[Ruby 문자열 슬라이싱](https://zetawiki.com/wiki/Ruby_문자열_슬라이싱_%28String_slice%29)

## 참고하세요
https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet