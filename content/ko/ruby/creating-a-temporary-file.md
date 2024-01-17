---
title:                "임시 파일 만들기"
html_title:           "Ruby: 임시 파일 만들기"
simple_title:         "임시 파일 만들기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 임시 파일 생성하기

우리가 소프트웨어를 만들 때, 때때로 우리는 우리의 코드에 임시 파일을 만듭니다. 임시 파일은 일시적으로 사용되는 파일로, 프로그램이 실행되는 동안 필요하지만 사용이 끝난 후 삭제됩니다. 이것이 임시 파일을 만드는 이유입니다.

## 어떻게 만드나요?

Ruby에서는 `Tempfile` 클래스를 사용하여 임시 파일을 생성할 수 있습니다. 예를 들어, 다음 코드를 보세요:

```Ruby
require 'tempfile'

file = Tempfile.new('temporary_file')

puts file.path
```

위의 코드를 실행하면, `Tempfile` 클래스가 제공하는 `#path` 메소드를 통해 임시 파일의 경로가 출력됩니다. 추가로, `Tempfile` 클래스는 생성된 임시 파일을 자동으로 삭제한다는 점이 특징입니다.

## 깊이 파헤쳐보기

임시 파일은 주로 데이터를 일시적으로 저장할 때 사용됩니다. 예를 들어, 파일을 다운로드하기 전에 임시 파일에 다운로드 데이터를 저장하고, 다운로드가 완료되면 임시 파일을 삭제할 수 있습니다. 또한, 임시 파일은 메모리가 부족한 경우에 일시적으로 사용할 수 있도록 하는 용도로도 사용됩니다.

이것 외에도 `Tempfile` 클래스를 사용하는 대신 우리가 직접 파일을 만들어서 임시 파일을 대신 사용할 수 있습니다. 하지만 이렇게 하면 파일의 삭제 관리를 직접 해야 하기 때문에, `Tempfile` 클래스를 사용하는 것이 더 편리합니다.

## 관련 자료

- [Tempfile 클래스 문서](https://ruby-doc.org/stdlib-2.6.3/libdoc/tempfile/rdoc/Tempfile.html)
- [Temporary file - Wikipedia](https://en.wikipedia.org/wiki/Temporary_file)