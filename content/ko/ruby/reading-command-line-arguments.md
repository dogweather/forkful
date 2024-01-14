---
title:    "Ruby: 컴퓨터 프로그래밍의 제목은 명령 줄 인수 읽기 입니다."
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 왜

명령 줄 인자를 읽는 것은 소프트웨어 개발자에게 매우 중요한 기술입니다. 이를 통해 사용자가 프로그램을 실행할 때 추가적인 설정이나 정보를 전달할 수 있으며, 더 유연하고 사용자 친화적인 프로그램을 만들 수 있습니다. 따라서 이 기술을 배우고 싶은 사람은 이 포스트를 읽어보세요!

## 어떻게

```ruby
#명령 줄 인자 읽기
input = ARGV[0]
puts "당신이 입력한 인자는 #{input}입니다."
```

위의 예시 코드와 같이, ARGV를 사용해 명령 줄 인자를 읽을 수 있습니다. 사용자가 입력한 인자는 배열 형태로 저장되며, 해당 배열의 첫 번째 요소가 실제 인자입니다. 즉, 위의 코드에서는 입력한 인자를 `input` 변수에 저장한 후, 해당 문자열을 출력하고 있습니다. 만약 여러 개의 인자를 읽고 싶다면, `ARGV` 배열의 다른 인덱스를 지정하면 됩니다.

## 깊이 파헤치기

명령 줄 인자를 읽는 방법에는 여러 가지가 있지만, 일반적으로 `ARGV` 라이브러리를 사용하는 것이 가장 흔합니다. 이 라이브러리를 사용하면 사용자를 위한 안내 메세지를 출력하거나, 인자가 잘못 입력되었을 때 대처하는 로직을 구현할 수도 있습니다. 또한 인자를 점검하는 로직을 추가해 사용자가 올바르지 않은 입력을 하더라도 프로그램이 멈추지 않도록 할 수도 있습니다. `ARGV` 외에도 `OptionParser`와 같은 라이브러리를 사용해 명령 줄 인자를 더욱 정교하게 다룰 수 있습니다.

## 또 다른 참조

- [명령 줄 인자 관련 루비 공식 문서](https://ruby-doc.org/core-2.7.1/ARGV.html)
- [유용한 루비 명령 줄 인자 팁과 트릭](https://blog.appsignal.com/2020/04/08/ruby-magic-parsing-command-line-arguments.html)
- [모든 명령 줄 인자를 읽고 출력하기](https://www.rubyguides.com/2017/12/ruby-command-line-arguments/)

## 참조

- [이페이스 블로그](https://blog.everface.co.kr)
- [루비 한국 사용자 그룹](https://ruby-korea.org)
- [루비 공식 홈페이지](https://www.ruby-lang.org/ko/)