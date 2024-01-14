---
title:                "Ruby: 컴퓨터 프로그래밍: 명령줄 인수 읽기"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인자를 읽는 것이 중요한 이유는 프로그래밍에서 유용한 기능 중 하나이기 때문입니다. 커맨드 라인 인자를 읽어들이면 사용자의 입력을 바탕으로 프로그램을 유연하게 동작시킬 수 있습니다.

## 어떻게

커맨드 라인 인자를 읽는 방법은 Ruby에서 간단하게 구현할 수 있습니다. ```ARGV```를 사용하여 입력된 인자를 배열로 저장하고, 인덱스를 통해 각 인자에 접근할 수 있습니다. 아래는 예시 코드입니다.

```Ruby
# 사용자 입력을 받아 출력하는 프로그램
puts "당신이 입력한 인자는 #{ARGV[0]}입니다."
```

위 예시 코드에서 ```ARGV[0]```은 첫 번째 인자를 나타냅니다. 여러 개의 인자가 있을 경우에는 ```ARGV[1]```, ```ARGV[2]```와 같은 방식으로 접근할 수 있습니다.

그리고 사용자의 입력이 정수나 실수일 경우에는 반드시 ```to_i```나 ```to_f```와 같은 메소드를 사용하여 데이터 타입을 변환해주는 것이 중요합니다. 예를 들어 사용자가 ```"3"```이라는 문자열을 입력했을 경우, ```to_i```를 사용하지 않고 바로 사용할 경우 문자열로 인식되어 계산이 불가능하므로 주의해야 합니다.

```Ruby
# 정수형으로 변환하여 계산하는 예시 코드
puts "당신이 입력한 숫자는 #{ARGV[0].to_i + 10}입니다."
```

위 예시 코드에서 ```ARGV[0]```이 ```"3"```이라면 출력은 ```13```이 될 것입니다.

## 깊이 살펴보기

커맨드 라인 인자를 읽는 것은 Ruby 뿐만 아니라 다른 프로그래밍 언어에서도 기본적인 기능으로 제공됩니다. 이를 통해 사용자의 입력을 받아 동적으로 프로그램을 제어할 수 있으며, 특정 동작을 실행하는데 필요한 인자를 알맞게 전달할 수 있습니다. 또한 작은 규모의 프로그램에서는 인터페이스를 구현하지 않고도 사용자와의 상호작용을 위해 이 기능을 활용할 수 있습니다.

## 더 알아보기

* [Learn Ruby the Hard Way - Exercise 13: Parameters, Unpacking, Variables](https://learnrubythehardway.org/book/ex13.html)
* [Command Line Arguments with Ruby](https://www.leighhalliday.com/command-line-arguments-ruby)
* [Parsing Command Line Arguments in Ruby](https://www.rubyguides.com/2018/05/ruby-arguments/)

## 관련 링크

* [Ruby 공식 홈페이지](https://www.ruby-lang.org/ko/)
* [Ruby 한국 사용자 그룹](http://ruby-korea.org/)
* [Ruby API 문서](https://rubyapi.org/)