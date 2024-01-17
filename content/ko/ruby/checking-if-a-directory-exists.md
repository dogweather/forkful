---
title:                "디렉토리가 존재하는지 확인하기"
html_title:           "Ruby: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

디렉토리가 존재하는지 확인하는 것은 프로그래머들이 자주 하는 작업입니다. 이는 코드에서 파일이나 디렉토리를 찾을 때 유용합니다. 

## 방법:

`Dir.exist?` 메소드를 사용하여 디렉토리가 존재하는지 체크할 수 있습니다. `true` 또는 `false` 값을 반환하며, 디렉토리가 존재하면 `true`가 반환됩니다. 예를 들어, 다음과 같이 사용할 수 있습니다.

```Ruby
if Dir.exist?('my_folder')
  puts 'my_folder exists!'
else
  puts 'my_folder does not exist.'
end
```

위의 코드를 실행하면 존재하는 디렉토리의 경우 `my_folder exists!`가 출력되고, 존재하지 않는 디렉토리의 경우 `my_folder does not exist.`가 출력됩니다.

## 깊이 파헤치기:

이 메소드는 Ruby 1.9부터 사용할 수 있습니다. 이전 버전의 Ruby에서는 `File.exists?` 메소드를 사용하여 파일 또는 디렉토리의 존재 여부를 확인할 수 있었습니다. 하지만 `Dir.exist?` 메소드는 메소드명 자체가 더 명확하고 직관적이기 때문에 더 선호됩니다.

`Dir.exist?` 메소드 외에도 `Dir.exist?` 메소드를 포함하는 `FileUtils` 모듈을 사용할 수도 있습니다. `FileUtils` 모듈에는 디렉토리를 생성하거나 삭제하는 등 다양한 유용한 기능들이 포함되어 있습니다.

## 관련 항목:

- [Ruby 문서 - Dir 클래스](https://ruby-doc.org/core-2.6.5/Dir.html)
- [Ruby 문서 - FileUtils 모듈](https://ruby-doc.org/stdlib-2.6.5/libdoc/fileutils/rdoc/FileUtils.html)