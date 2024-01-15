---
title:                "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
html_title:           "Ruby: 컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령 줄 인수 읽기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

여기에서 우리는 커맨드 라인 인수에 대해 배우게 될 것입니다. 커맨드 라인 인수는 Ruby 프로그램에서 중요한 부분입니다. 우리는 이 문제를 해결하기 위해 어떤 방법으로 코드를 작성할 수 있는지 살펴볼 것입니다.

## 어떻게

커맨드 라인 인수는 Ruby 프로그램에서 사용자와 상호 작용하는 중요한 방법 중 하나입니다. 사용자는 프로그램을 실행할 때 추가적인 정보를 전달할 수 있는데요. 이 정보는 우리 프로그램이 다른 방법으로 실행될 수 있도록 제공됩니다. 사용자로부터 커맨드 라인 인수를 받기 위해서는 다음과 같은 코드를 작성할 수 있습니다.

```Ruby
# 커맨드 라인 인수를 받는 방법
ARGV.each do |arg|
  puts "Received argument: #{arg}"
end
```

이제 위의 코드를 작성한 후, 우리는 다음과 같이 우리 프로그램을 실행할 수 있습니다.

```
ruby program.rb hello world
```

위의 코드를 실행하면, "Received argument: hello"와 "Received argument: world"가 출력됩니다. 즉, 커맨드 라인 인수는 우리에게 많은 유연성을 제공합니다. 우리는 입력된 인수에 따라 프로그램의 동작을 다르게 설정할 수 있습니다. 예를 들어, 위의 코드에서 우리는 `if` 문을 사용하여 각각의 인수에 따라 다른 동작을 수행할 수 있습니다.

## 딥 다이브

Ruby의 `ARGV` 배열은 우리가 커맨드 라인 인수를 다루는 가장 간단한 방법 중 하나입니다. 그러나 더 복잡한 상황에서는 다른 방법으로 커맨드 라인 인수를 다룰 수 있습니다. 예를 들어, `OptionParser` 클래스를 사용하면 더 많은 옵션을 제공하고 더 복잡한 커맨드 라인 인수를 다룰 수 있습니다.

## 보기

- [Ruby의 ARGV 배열 문서](https://ruby-doc.org/core-3.0.1/ARGV.html)
- [OptionParser 클래스 문서](https://ruby-doc.org/stdlib-3.0.1/libdoc/optparse/rdoc/OptionParser.html)