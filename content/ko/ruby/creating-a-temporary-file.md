---
title:                "Ruby: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜

일시적인 파일을 생성하는 것이 유용한 이유는 여러 가지가 있습니다. 가장 일반적인 이유는 임시적으로 데이터를 저장하고 데이터 처리 작업을 수행하는 데 필요한 사전 작업을 수행하는 것입니다. 또한 많은 프로그램에서 임시 파일을 사용하여 메모리를 관리하고 프로그램 실행 속도를 향상시키는 데 도움이 됩니다.

## 하는 방법

루비는 임시 파일을 만드는 데 사용할 수 있는 몇 가지 내장 라이브러리를 제공합니다. 그 중에서도 가장 일반적인 방법은 `Tempfile` 클래스를 사용하는 것입니다. 다음은 임시 파일을 생성하고 데이터를 쓰는 간단한 예제입니다.

```Ruby
require 'tempfile' // 임시 파일 라이브러리 호출

// 임시 파일 생성
file = Tempfile.new('example')

// 데이터 쓰기
file.write("Hello World!")

// 임시 파일 닫기
file.close

// 생성한 임시 파일 읽기
content = File.read(file.path)

// 결과 출력
puts content // "Hello World!"
```

또 다른 방법으로는 `Kernel#open` 메서드를 사용하는 것입니다. 이 메서드는 임시 파일 생성과 동시에 데이터를 쓰는 작업을 한 번에 수행할 수 있습니다.

```Ruby
// 코드를 한 줄로 줄여 간단하게 작성 가능
content = File.open("example.txt", "w+") { |file| file.write("Hello World!") }

// 결과 출력
puts content // "Hello World!"
```

## 깊게 들어가기

임시 파일을 생성하는 가장 기본적인 방법은 `Tempfile` 클래스를 사용하는 것입니다. 이 클래스는 기본적으로 `File` 클래스를 상속하며 임시 파일을 생성하는 데 필요한 모든 기능을 제공합니다. 이 클래스는 내부적으로 시스템의 임시 디렉터리에 파일을 생성하고 프로그램이 종료될 때 자동으로 파일을 삭제합니다. 또한 파일을 열고 읽고 쓰는 데 필요한 모든 메서드를 제공하여 파일 작업을 쉽게 수행할 수 있도록 도와줍니다.

## 참고

- [Ruby Tempfile 클래스 문서](https://ruby-doc.org/core-2.7.1/Tempfile.html)
- [Ruby Kernel#open 메서드 문서](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-open)
- [Ruby File 클래스 문서](https://ruby-doc.org/core-2.7.1/File.html)