---
title:    "Ruby: 텍스트 파일 작성하기"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

Ruby 프로그래밍 블로그/텍스트 파일 쓰는 법


## 왜

텍스트 파일을 쓰는 이유는 여러 가지가 있습니다. 대표적으로 데이터를 보관하거나, 프로그램을 실행시키는 도중에 임시적인 정보를 저장하기 위해 사용됩니다. 또한, 소스 코드나 텍스트 문서를 저장하는 용도로도 많이 활용됩니다.

## 쓰는 방법

텍스트 파일을 쓰기 위해서는 Ruby의 File 클래스를 사용해야 합니다. 아래는 간단한 예시 코드입니다:

```Ruby
# 쓰기용 파일을 열기 위해 File.open 메소드를 사용합니다.
File.open("text_file.txt", "w") do |file|
    # 파일에 문자열을 쓰는 메소드는 write입니다.
    file.write("안녕하세요, 여러분!")
    # 여러 줄을 한 번에 쓰고 싶다면 puts 메소드를 사용할 수 있습니다.
    file.puts("이번에는")
    file.puts("여러 줄도")
    file.puts("한 번에 작성해보세요!")
end

# 쓴 내용을 읽어와 출력해봅시다.
puts File.read("text_file.txt")
```

위 코드를 실행하면 다음과 같은 결과가 나옵니다:

```
안녕하세요, 여러분!
이번에는
여러 줄도
한 번에 작성해보세요!
```

## 깊게 들어가기

위에서 소개한 코드는 가장 기본적인 텍스트 파일 쓰기 방법입니다. 하지만 여러분은 파일을 여는 모드를 다르게 설정하거나, StringIO 클래스를 사용하여 메모리에 임시 파일을 생성하고 사용할 수도 있습니다. 또한, 파일을 읽는(read) 메소드와 쓰는(write) 메소드뿐만 아니라, 파일을 닫는(close) 메소드도 알아두는 것이 좋습니다.

## 더 알아보기

* [Ruby File 클래스 공식 문서](https://ruby-doc.org/core-2.5.1/File.html)
* [Ruby StringIO 클래스 공식 문서](https://ruby-doc.org/stdlib-2.5.1/libdoc/stringio/rdoc/StringIO.html)
* [파일 입출력에 대한 자세한 설명](https://www.rubyguides.com/2015/05/working-with-files-ruby/) 

## 더 보기

* [Markdown 언어에 대한 자세한 설명](https://daringfireball.net/projects/markdown/syntax)
* [이전에 작성한 Ruby 프로그래밍 블로그/텍스트 파일 읽는 법](https://github.com/nereusong/ruby_programming_blog/blob/master/reading-text-file.md)