---
title:                "Ruby: 텍스트 검색 및 교체하기"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜?
텍스트를 검색하고 대체하는 작업을 왜 해야 할까요? 많은 컴퓨터 프로그래밍 작업은 텍스트 기반으로 이루어지기 때문입니다. 따라서 특정 텍스트를 일괄적으로 검색하고, 해당 텍스트를 다른 내용으로 대체하는 것은 유용하고 효율적입니다. 특히, 대규모 코드 베이스에서 특정 변수 이름이나 함수 호출 이름을 변경해야 할 때 매우 유용합니다.

## 어떻게 할까요?
검색하고 대체하는 작업은 Ruby의 String 클래스와 정규식을 사용하여 쉽게 할 수 있습니다. 우선, 아래와 같이 `gsub` 메소드를 사용하여 특정 텍스트를 다른 내용으로 대체할 수 있습니다.

```Ruby
puts "Hello, world!".gsub("world", "Korea") #=> "Hello, Korea!"
```

또한, 정규식을 사용하여 더욱 강력한 검색 및 대체 작업을 수행할 수 있습니다. 정규식을 사용하면 패턴을 이용하여 더욱 정교하게 텍스트를 검색하고, 해당 패턴에 일치하는 부분을 다른 내용으로 대체할 수 있습니다.

```Ruby
puts "The quick brown fox jumps over the lazy dog".gsub(/[aeiou]/, '*') #=> "Th* q**ck br*wn f*x j*mps *v*r th* l*zy d*g"
```

## 깊게 들어가보기
Ruby의 String 클래스에는 `gsub` 메소드 외에도 `sub`과 `gsub!` 메소드가 있습니다. `sub`은 첫 번째 일치하는 부분만 대체하는 반면, `gsub`은 모든 일치하는 부분을 대체합니다. 또한, `gsub!` 메소드는 대체한 결과를 인스턴스에 저장하여 기존 문자열을 변경합니다.

또한, 정규식을 사용하여 치환 문자열을 동적으로 생성할 수 있습니다. 예를 들어, 아래와 같이 패턴 매치 결과에 따라 대체할 문자열을 다르게 지정할 수 있습니다.

```Ruby
puts "Bob is a great programmer".gsub(/Bob|programmer/, "Bill" => "CEO", "programmer" => "developer") #=> "Bill is a great CEO"
```

## 알아두면 유용한 팁
검색 및 대체 작업을 수행할 때, 정규식의 캡처 그룹을 활용하면 더욱 다양한 작업을 할 수 있습니다. 캡처 그룹은 괄호로 묶인 부분을 의미하며, 일치하는 텍스트를 추출할 수 있습니다. 이를 활용해 텍스트를 다양하게 재구성할 수 있습니다.

또한, 정규식의 match 메소드를 사용하면 해당 텍스트가 정규식에 일치하는지 여부를 빠르게 확인할 수 있습니다. 이를 활용하여 대소문자를 무시하는 검색을 수행할 수 있습니다.

# 또 다른 예제
- [Ruby의 정규식 사용 예제](https://ruby-doc.com/docs/Tutorial/part_02/phrase.html)
- [Ruby에서 문자열 조작하기](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Ruby의 정규식과 치환](https://docs.ruby-lang.org/en/2.0.0/String.html#method-i-gsub-21)

# 참고자료
- [Ruby 문서 - String 클래스](https://ruby-doc.com/core-2.7.0/String.html)
- [Ruby 문서 - 정규식](https://ruby-doc.com/docs/ProgrammingRuby/html/tut_stdtypes.html