---
title:                "Ruby: 컴퓨터 프로그래밍에서 명령줄 인수 읽기"
simple_title:         "컴퓨터 프로그래밍에서 명령줄 인수 읽기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

커맨드 라인 인수를 읽는 것에 대해 궁금하다면, 이 블로그 포스트를 통해 더 많은 정보를 얻을 수 있습니다. 프로그래밍에 익숙한 사람이라면 커맨드 라인 인수를 사용하는 것이 유용한 방법이 될 것입니다.

## 어떻게

커맨드 라인 인수를 읽는 방법을 살펴보겠습니다. 예제 코드를 통해 실제로 어떻게 동작하는지 알아보겠습니다.

```Ruby
# 커맨드 라인 인수를 배열로 읽기

arguments = ARGV
puts arguments 

# 커맨드 라인 인수 출력하기

ruby script.rb "Hello, world!"
# => "Hello, world!"
```

위의 코드에서 `ARGV`는 사용자가 커맨드 라인에서 입력한 모든 인수를 담은 배열이 됩니다. 이를 통해 사용자가 입력한 인수를 코드 안에서 활용할 수 있습니다. 또한 예제 코드를 통해 어떻게 커맨드 라인 인수를 출력할 수 있는지도 알 수 있습니다.

## 딥 다이브

커맨드 라인 인수를 읽는 것은 프로그래밍에서 매우 유용합니다. 사용자가 입력한 인수를 활용하여 프로그램의 동작을 변화시킬 수 있습니다. 예를 들어, 파일의 이름을 입력받아 해당 파일을 읽어오는 프로그램을 만들 수 있습니다. 또는 사용자가 입력한 옵션에 따라 프로그램의 기능을 변경할 수도 있습니다.

커맨드 라인 인수를 읽을 때 주의할 점은, 사용자가 입력한 모든 인수가 문자열 형태로 저장되어 있다는 것입니다. 따라서 사용자가 입력한 값을 정수나 실수 등으로 변환하여 사용해야 한다면 추가적인 작업이 필요합니다.

## 더 알아보기

- [커맨드 라인 인수에 대한 레일스 가이드 문서](https://guides.rubyonrails.org/command_line.html)
- [커맨드 라인 인수를 처리하는 다른 방법](https://www.rubyguides.com/2019/08/ruby-command-line-arguments/)
- [커맨드 라인 인수에 관한 RubyDoc 페이지](https://ruby-doc.org/core-2.7.2/ARGV.html)

## 더 알아보기