---
title:                "텍스트 파일 읽기"
html_title:           "Ruby: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜

텍스트 파일을 읽는 방법을 알려주는 것이 여러분에게 도움이 될 것입니다. 이를 통해 여러분은 더 많은 데이터를 다룰 수 있고, 코드 작성에 유용한 정보를 얻을 수 있습니다.

## 어떻게

텍스트 파일을 읽는 것은 아주 쉬운 작업입니다. 다음 예제 코드를 따라해보세요.

```Ruby
file = File.open("sample.txt") # 읽을 파일의 경로를 정확하게 입력하세요.

while(line = file.gets) # 파일에서 한 줄씩 읽어옵니다.
  puts line # 그 줄을 출력합니다.
end

file.close # 파일을 닫아 줍니다.
```

위 코드를 실행하면, 해당 파일의 모든 내용이 출력됩니다. 만약 특정 단어를 검색해서 출력하고 싶다면, 다음과 같이 코드를 수정할 수 있습니다.

```Ruby
file = File.open("sample.txt") # 읽을 파일의 경로를 정확하게 입력하세요.
search_word = "Ruby" # 검색할 단어를 지정합니다.

while(line = file.gets) # 파일에서 한 줄씩 읽어옵니다.
  if line.include?(search_word) # 현재 줄에 검색할 단어가 포함되어 있다면
    puts line # 그 줄을 출력합니다.
  end
end

file.close # 파일을 닫아 줍니다.
```

위 코드는 해당 파일에서 입력한 단어가 포함된 모든 줄을 출력합니다.

## Deep Dive

텍스트 파일은 여러분이 처리해야 할 대량의 데이터를 포함할 수 있습니다. 이를 처리하기 위해 Ruby는 여러 가지 방법을 제공합니다. 위 예제에서는 파일을 한 줄씩 읽어오지만, `read` 메서드를 사용하면 파일 전체를 읽어서 하나의 문자열로 반환할 수도 있습니다. 또는 `each_byte` 메서드를 사용하면 파일을 바이트 단위로 하나씩 읽을 수도 있습니다.

이 외에도 Ruby는 텍스트 파일 다루기에 유용한 많은 메서드를 제공하고 있으니, 여러분은 필요에 따라서 찾아보고 활용할 수 있습니다.

## See Also

- [Ruby 파일 다루기](https://www.daleseo.com/ruby-file/)
- [Ruby 문서 - 파일 클래스](https://ruby-doc.org/core-3.0.0/File.html)
- [Ruby 파일 입출력 가이드](https://www.rubyguides.com/ruby-file-io/)