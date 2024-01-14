---
title:                "Ruby: 임시 파일 만들기"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 왜

새파일을 만드는 작업은 임시적인 코드 또는 데이터를 저장하고 사용하는 데 유용합니다. 파일 시스템을 사용하여 데이터를 임시적으로 저장하면 메모리에 부담이 덜 가고 무결성이 보장되므로 안전합니다.

## 방법

```Ruby
# 새로운 임시 파일을 만들기 위해 Tempfile 클래스를 사용합니다
123temp_file = Tempfile.new

# 데이터를 임시 파일에 쓰기 위해 write 메소드를 사용합니다
temp_file.write("안녕하세요, Ruby 프로그래밍을 배우고 있습니다.")

# 임시 파일에서 데이터를 읽기 위해 rewind 메소드를 사용합니다
temp_file.rewind

# 데이터를 임시 파일에서 읽어옵니다
puts temp_file.read

# 파일 시스템에서 사용한 임시 파일을 제거합니다
temp_file.close
temp_file.unlink
```

**출력:**
안녕하세요, Ruby 프로그래밍을 배우고 있습니다.

## 깊이 파고들기

Tempfile 클래스를 사용하여 새로운 임시 파일을 만들 때, 파일 이름에 대한 계획되지 않은 가정이 없습니다. 이는 보안 상의 이유로 인해 안전합니다. 이러한 임시 파일은 사용을 마친 후 Tempfile 클래스에 의해 자동으로 삭제됩니다.

Tempfile 클래스는 File 클래스와 유사한 메소드를 제공하지만, 파일을 일부가 아닌 모든 내용을 메모리 버퍼에 쓰므로, 파일의 크기가 크거나 늘어나는 도중 크래시가 발생할 수 있습니다. 이를 방지하기 위해 메모리 버퍼가 아닌 파일 시스템을 사용하여 데이터를 저장하는 것이 좋습니다.

# 더 알아보기

[Tempfile 클래스 공식 문서](https://ruby-doc.org/stdlib-2.7.0/libdoc/tempfile/rdoc/Tempfile.html)

[Ruby 파일 입출력 기초](https://code-maven.com/ruby-i-o-basics)

[Ruby 파일 분석기](http://rubyschool.us/blog/function-search-for-text-in-the-file/)

[블록으로 데이터베이스 연결하는 방법](https://www.rubyguides.com/2019/07/hash-vs-dictionary-ruby/)