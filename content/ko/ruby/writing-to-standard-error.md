---
title:                "표준 오류에 쓰는 방법"
html_title:           "Ruby: 표준 오류에 쓰는 방법"
simple_title:         "표준 오류에 쓰는 방법"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 왜

이 글에서는 Ruby 언어에서표준 에러에 쓰는 방법을 배우게 됩니다. 표준 에러는 프로그램 실행 중에 발생하는 오류 메시지를 나타내는데, 오류를 디버깅하고 코드를 개선하는 데 매우 중요합니다.

## 어떻게 하면 될까요?

우선 `STDERR.puts` 메소드를 사용하여 표준 에러에 내용을 출력할 수 있습니다. 예를 들어, 다음과 같이 코드를 작성해보세요.

```ruby
STDERR.puts "이것은 표준 에러에 출력됩니다."
```

위 코드를 실행하면, 표준 에러에 해당 내용이 출력될 것입니다. 이를 통해 오류 메시지를 더욱 자세하게 파악할 수 있고, 디버깅을 쉽게 할 수 있습니다.

## 깊이 있는 내용

Ruby에는 표준 에러를 다루기 위한 몇 가지 방법이 있습니다. `STDERR.puts` 외에도 `STDERR.print`나 `STDERR.write` 메소드를 사용할 수 있습니다. 이들 각각은 다른 방식으로 표준 에러에 내용을 출력합니다. 또한 `STDERR` 객체 자체를 사용하여, 편리하게 표준 에러를 다룰 수 있습니다.

# 참고

- [Ruby 공식 문서](https://ruby-doc.org/core-3.0.1/IO.html#class-IO-label-Standard+Streams)
- [RubyMonk 에러 처리 강의](https://www.rubymonk.com/learning/books/4-ruby-primer-ascent/crash-course-in-ruby#ruby-errors-and-exceptions)