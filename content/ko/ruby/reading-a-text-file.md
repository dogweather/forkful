---
title:                "Ruby: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

# 왜Text file을 읽는가?

많은 프로그래머들이 텍스트 파일을 읽어야 하는 일상적인 작업을 맡게 됩니다. 이를 위해 여러 가지 이유가 있을 수 있지만 가장 일반적인 이유는 파일로부터 데이터를 추출할 필요가 있기 때문입니다. 예를 들어, 특정 문자열을 포함하는 텍스트 파일을 찾아야 할 때, 파일을 읽어 검색할 수 있습니다. 또는 파일의 내용을 분석하여 데이터를 추출하고 가공할 수도 있습니다. 어떤 이유든간에, 텍스트 파일을 읽는 것은 프로그래밍에서 중요한 부분입니다.

## 어떻게 Text file을 읽는가?

Ruby에서는 `File` 클래스를 이용하여 텍스트 파일을 읽을 수 있습니다. 가장 간단한 방법은 `File.read` 메서드를 사용하는 것입니다. 이 메서드는 파일의 모든 내용을 하나의 문자열로 반환합니다.

```Ruby
file_content = File.read("text.txt")
puts file_content
```

만약 파일을 한 줄씩 읽고 싶다면, `File::foreach` 메서드를 사용할 수 있습니다. 이 메서드는 파일의 각 줄을 함수의 인자로 전달하여 처리합니다.

```Ruby
File.foreach("text.txt") do |line|
  puts line
end
```

또 다른 방법은 파일 객체를 생성하여 `readline` 메서드를 이용하는 것입니다. 이 방법은 파일을 한 줄씩 읽는 것이 아니라, 직접 파일의 내용을 다룰 수 있게 해줍니다.

```Ruby
file = File.open("text.txt")
line = file.readline
puts line
```

## Deep Dive

Ruby에서는 `File` 클래스 이외에도 `IO` 클래스를 통해 파일을 다룰 수 있습니다. `IO` 클래스는 파일 입력과 출력을 담당하는 모든 클래스의 부모 클래스입니다. 그래서 텍스트 파일을 읽는 모든 방법은 `IO` 클래스의 메서드를 이용하는 것입니다.

`read` 메서드는 파일 전체를 읽을 수 있지만, 큰 파일을 읽거나, 파일을 한 번에 읽는 것이 불가능한 상황일 수도 있습니다. 이때는 `readline` 메서드를 사용하여 파일을 분할해서 읽을 수 있습니다. 또한, 파일에 대해 `each` 메서드를 호출하면 파일의 내용을 반복적으로 처리할 수 있습니다.

See Also
- [File - Ruby 문서] (https://ruby-doc.org/core-3.0.2/File.html)
- [IO - Ruby 문서] (https://ruby-doc.org/core-3.0.2/IO.html)
- [How to Read Files in Ruby] (https://www.tutorialspoint.com/ruby/ruby_input_output.htm)