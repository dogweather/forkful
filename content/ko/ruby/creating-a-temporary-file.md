---
title:                "임시 파일 생성하기"
html_title:           "Python: 임시 파일 생성하기"
simple_title:         "임시 파일 생성하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 왜 & 왜케: 

임시 파일(temporary file)을 만드는 것은 컴퓨터의 디스크에 임시적으로 데이터를 저장하는 작업입니다. 프로그래머는 디버깅, 대용량 데이터 처리 등의 상황에서 이를 사용합니다.

## 사용법:

Ruby에서는 `Tempfile` 라이브러리를 사용하여 임시 파일을 만들 수 있습니다.

```Ruby
require 'tempfile'

Tempfile.create('myTempFile') do |temp|
  temp.write 'hello, world!'
  temp.rewind
  puts temp.read #=> 결과: hello, world!
end
```

이 코드는 'myTempFile'이라는 임시 파일을 생성하고, 이 파일에 'hello, world!'라는 텍스트를 기록한 뒤 읽는 예시입니다.

## 학술적 고찰:

임시 파일 기능은 초기 UNIX 시스템에서 데이터 처리의 유용성 때문에 생성되었습니다. Ruby에서는 'tmp' 모듈을 사용하였고, 이후 1.8.7 버전에서 `Tempfile` 라이브러리로 변경되었습니다.

`Tempfile.create` 대신 `Tempfile.new`를 사용할 수도 있지만 `Tempfile.create`가 더 안전하고 효율적입니다. 왜냐하면 `Tempfile.new`는 파일을 생성하기만 하며 삭제를 보장하지 않기 때문입니다.

`Tempfile.create`는 블록이 끝나자마자 임시 파일을 삭제하는 것을 보장하므로 임시 파일을 안전하게 관리할 수 있습니다.

## 참고자료:

Ruby `Tempfile` 문서: https://ruby-doc.org/stdlib-2.5.1/libdoc/tempfile/rdoc/Tempfile.html 

더 깊이 이해하기 위한 출처: https://learning.oreilly.com/library/view/ruby-cookbook/0596523696/ch06s95.html