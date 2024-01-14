---
title:                "Ruby: 정규식 사용하기"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜? 

정규 표현식을 사용하면 문자열을 검색하고, 바꾸고, 조작하는 데 유용합니다. 이는 코딩 작업을 더 효율적으로 만들어줍니다.

## 사용 방법 How To 

정규 표현식은 `/` 기호로 시작해 `/` 기호로 끝나는 패턴을 사용하여 이루어집니다. 다음 예제를 봅시다.

```Ruby
phrase = "안녕하세요! 반가워요? Hello! Nice to meet you."

phrase.scan(/안녕|반가워|Hello|Nice/) do |match|
    p match
end
```

위 코드의 출력 결과는 다음과 같습니다.

```
안녕
반가워
Hello
Nice
```

한 줄의 코드로 모든 패턴을 찾을 수 있습니다.

```Ruby
phrase = "Can't stop, won't stop!"

phrase.gsub!(/t\w+/, '**')

puts phrase
```

출력 결과는 다음과 같습니다.

```
Can't **, won't **!
```

정규 표현식을 사용해 나중에 많은 코딩시간을 단축할 수 있습니다. 그래서 많은 개발자들이 자주 사용합니다.

## 깊이 있는 내용 Deep Dive 

정규 표현식을 배우려면 좀 더 깊게 파헤쳐봐야합니다. 다음 링크들을 참고하면 감을 잡을 수 있습니다.

- [Ruby Regular Expressions](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Advanced Regular Expressions in Ruby](https://stackoverflow.com/questions/22937618/advanced-regular-expressions-in-ruby)

## 관련 링크들 See Also 

- [정규 표현식 배우기](https://rubykoans.com)
- [정규 표현식 정리 자료](http://www.rexegg.com)
- [루비 공식 문서 - 정규 표현식](https://ruby-doc.org/core-2.7.2/Regexp.html)