---
title:                "명령줄 인수 읽기"
html_title:           "Arduino: 명령줄 인수 읽기"
simple_title:         "명령줄 인수 읽기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
명령 줄 인수를 읽는 것은 프로그래머가 실행 시에 추가 데이터를 프로그램에 제공할 수 있게 하는 방법입니다. 이러한 기능은 데이터의 맥락에 맞춰 동적으로 동작하는 프로그램을 만드는 데 중요합니다.

## 어떻게:
```ruby
# 아래 예제는 명령 줄 인수를 사용하는 간단한 코드입니다.
ARGV.each do |argument|
  puts "Received argument: #{argument}"
end
```
위 코드를 사용해 아래와 같이 실행하면:
```shell
ruby script.rb first_arg second_arg
```
다음과 같은 출력을 볼 수 있습니다:
```shell
Received argument: first_arg
Received argument: second_arg
```

## 깊이 분석
명령 줄 인수를 읽는 기능은 UNIX 시스템에서 오래 전부터 사용되었습니다. 이는 유연한 소프트웨어 개발을 가능케 했습니다. 루비에서도 이를 유지하고 있습니다.

대안으로, 대화형 프롬프트나 구성 파일을 사용하여 동일한 용도로 사용할 수 있습니다. 하지만 명령 줄 인수는 실행할 때마다 코드를 변경하지 않고도 프로그램의 동작을 쉽게 바꿀 수 있는 능력을 제공합니다.

ARGV는 배열로 표현되며, 각 요소는 스페이스로 구분된 문자열로 채워집니다. 이 첫 번째로 숫자를 사용하여 인덱싱될 수 있습니다.

## 참고 자료
* [Command Line Arguments in Ruby - Stack Overflow](https://stackoverflow.com/questions/4840688/command-line-arguments-in-ruby)