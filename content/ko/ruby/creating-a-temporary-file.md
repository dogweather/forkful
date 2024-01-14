---
title:    "Ruby: 임시 파일 생성하기"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜
임시 파일을 만들기 위해서 어떻게 하는지 궁금했던 적이 있나요? 이 블로그 포스트에서는 임시 파일을 생성하는 이유에 대해 알아보고, 간단한 코드 예제와 함께 실제로 어떻게 만드는지 살펴보겠습니다.

## 어떻게
임시 파일을 생성하는 방법은 간단합니다. Ruby 내장 함수인 `Tempfile`을 사용하면 됩니다. 아래의 코드 예제를 참고해보세요.

```Ruby
require 'tempfile'

temp_file = Tempfile.new("sample") # "sample" 이름의 임시 파일 생성
temp_file.write("Hello, world!") # 파일에 내용 쓰기
puts temp_file.path # 임시 파일의 경로 출력

temp_file.close # 파일 닫기
temp_file.unlink # 파일 삭제
```

위 코드를 실행하면 `"Hello, world!"`가 적힌 임시 파일이 생성됩니다. 코드에 적힌 주석을 참고하시면 더 자세한 설명을 볼 수 있습니다.

## 딥 다이브
위에서 간단하게 소개한 `Tempfile` 함수는 실제로는 다른 여러가지 기능을 제공합니다. 임시 파일이 여러 명의 사용자에 의해 동시에 읽고 쓰일 때 발생하는 문제를 해결하기 위한 `Tempfile.open` 메소드도 있습니다. 또한, `Tempfile`은 임시 파일 뿐만 아니라 임시 디렉토리를 생성하는 기능도 제공합니다. 여러분이 원하는 내용에 따라 더 깊이 공부해보세요!

## 참고
[Tempfile 공식 문서(영어)](https://ruby-doc.org/stdlib-2.7.2/libdoc/tempfile/rdoc/Tempfile.html)
[Tempfile 클래스 소개 블로그 포스트(영어)](https://mixandgo.com/blog/how-to-use-ruby-tempfile-class)