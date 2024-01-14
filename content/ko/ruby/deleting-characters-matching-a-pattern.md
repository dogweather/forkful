---
title:                "Ruby: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜
문자 패턴과 일치하는 문자를 삭제하는 작업을 수행하는 이유는 무엇일까요? 그 이유를 간단히 알아봅시다.

## 어떻게
문자 패턴과 일치하는 문자를 삭제하는 방법에 대해 알아보겠습니다. 코드 블록을 사용하여 샘플 코드와 출력 결과를 함께 확인해보세요.

```Ruby
# 패턴으로 "a"를 포함하는 모든 문자 삭제하기
string = "apple, banana, cat, dog, elephant"
new_string = string.gsub(/a/, "")
puts new_string
# 출력: "pple, bn, c, dog, elephnt"
```

```Ruby
# 패턴으로 숫자를 포함하는 모든 문자 삭제하기
string = "hello 123 world"
new_string = string.gsub(/\d/, "")
puts new_string
# 출력: "hello world"
```

## 깊게 파헤치기
문자 패턴과 일치하는 문자를 삭제하는 더 깊은 내용을 살펴봅시다. 정규 표현식을 사용하여 패턴을 지정하고, gsub 메서드를 이용하여 문자를 치환하는 방식으로 작업을 수행합니다. 이를 통해 우리는 특정 패턴을 가지고 있는 문자열을 더 쉽게 삭제할 수 있습니다.

## 더 알아보기

[정규 표현식에 대한 자세한 설명](https://ruby-doc.org/core-2.6.3/Regexp.html)

[gsub 메서드의 사용 예시](https://www.rubyguides.com/2019/02/ruby-gsub-method/)

[문자열 메서드에 대한 자세한 설명](https://ruby-doc.org/core-2.6.3/String.html)

## 관련 링크

[Ruby 공식 문서](https://www.ruby-lang.org/ko/documentation/)

[Ruby 관련 블로그들](https://rubykr.github.io/)

[Ruby 온라인 커뮤니티](https://rubyko.github.io/)