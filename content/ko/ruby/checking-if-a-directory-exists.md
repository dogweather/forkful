---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "C#: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?

디렉토리가 존재하는지 확인하는 것은 특정 디렉토리의 존재를 체크하는 작업입니다. 프로그래머들은 이 작업을 통해 읽기/쓰기 작업 전에 해당 위치에 디렭토리가 실제로 있는지 확인하여 에러를 방지합니다.

## 어떻게 할까?

Ruby에서 디렉토리 존재 확인은 다음과 같이 수행할 수 있습니다.

```Ruby
require 'fileutils'

path = "/path/to/directory"

if Dir.exist?(path)
  puts "The directory exists."
else
  puts "The directory does not exist."
end
```
우리가 원하는 디렉토리의 경로를 `path`에 입력하면, 존재하는지를 확인합니다. 디렉토리가 존재하면 "The directory exists."라고 출력되고, 그렇지 않으면 "The directory does not exist."라고 출력됩니다.

## 깊이 들어가보기

디렉토리의 존재 여부를 확인하는 코드는 파일 시스템 성능에 기반하여 작성된 코드입니다. 이는 Ruby가 처음 개발되었을 때부터 그 대부분의 운영 체제와 호환되도록 설계되었습니다.

대안으로 `File.directory?(path)`를 사용할 수도 있습니다. 이 메서드는 입력된 경로가 디렉토리면 true를, 그렇지 않으면 false를 반환합니다. 하지만 이는 디렉토리 뿐만 아니라 파일의 존재도 확인합니다. 따라서 이메서드는 디렉토리의 존재 여부만을 확인하려는 목적에는 적합하지 않습니다.

`Dir.exist?` 메서드는 Ruby의 입출력 중 하나인 Dir 클래스의 인스턴스 메서드입니다. 이 메서드는 특정 디렉토리의 존재 여부를 확인하는 역할을 합니다.

## 참고하기

다음과 같은 레퍼런스에서 관련 내용을 더 찾아볼 수 있습니다.
- Ruby 공식 문서: [Dir.exist?](https://ruby-doc.org/core-2.7.1/Dir.html#method-c-exist-3F), [File.directory?](https://ruby-doc.org/core-2.7.1/File.html#method-c-directory-3F)
- Stack Overflow: [Check if a directory exists in a shell script](https://stackoverflow.com/questions/59838/check-if-a-directory-exists-in-a-shell-script)