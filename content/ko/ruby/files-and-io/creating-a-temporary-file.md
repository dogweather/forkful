---
date: 2024-01-20 17:41:17.668707-07:00
description: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uC784\uC2DC \uD30C\uC77C\
  \uC744 \uB9CC\uB4DC\uB294 \uAC83\uC740 \uB370\uC774\uD130\uB97C \uC77C\uC2DC\uC801\
  \uC73C\uB85C \uC800\uC7A5\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC8FC\uB85C \uACE0\uC815\uB41C \uC800\
  \uC7A5\uC18C\uC758 \uC624\uBC84\uD5E4\uB4DC\uB97C \uD53C\uD558\uAC70\uB098, \uC77C\
  \uD68C\uC131 \uB370\uC774\uD130\uB97C \uCC98\uB9AC\uD560 \uB54C \uC774\uB97C \uD65C\
  \uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.948138-06:00'
model: gpt-4-1106-preview
summary: "\uD504\uB85C\uADF8\uB798\uBC0D\uC5D0\uC11C \uC784\uC2DC \uD30C\uC77C\uC744\
  \ \uB9CC\uB4DC\uB294 \uAC83\uC740 \uB370\uC774\uD130\uB97C \uC77C\uC2DC\uC801\uC73C\
  \uB85C \uC800\uC7A5\uD558\uAE30 \uC704\uD574 \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC8FC\uB85C \uACE0\uC815\uB41C \uC800\uC7A5\
  \uC18C\uC758 \uC624\uBC84\uD5E4\uB4DC\uB97C \uD53C\uD558\uAC70\uB098, \uC77C\uD68C\
  \uC131 \uB370\uC774\uD130\uB97C \uCC98\uB9AC\uD560 \uB54C \uC774\uB97C \uD65C\uC6A9\
  \uD569\uB2C8\uB2E4."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
프로그래밍에서 임시 파일을 만드는 것은 데이터를 일시적으로 저장하기 위해 사용합니다. 프로그래머들은 주로 고정된 저장소의 오버헤드를 피하거나, 일회성 데이터를 처리할 때 이를 활용합니다.

## How to: (방법)
```Ruby
require 'tempfile'

# 임시파일 생성
temp_file = Tempfile.new('tempfile_demo')
puts "임시 파일 경로: #{temp_file.path}"

# 임시파일에 내용 쓰기
temp_file.write('안녕하세요, Ruby!')
temp_file.rewind

# 임시파일로부터 내용 읽기
puts "파일 내용: #{temp_file.read}"

# 임시파일 닫고 삭제
temp_file.close
temp_file.unlink

# 결과 확인
puts "파일 삭제됨!" unless File.exist?(temp_file.path)
```

Sample Output:
```
임시 파일 경로: /tmp/tempfile_demo20130822-8379-1w0fer9
파일 내용: 안녕하세요, Ruby!
파일 삭제됨!
```

## Deep Dive (심층 탐구)
Ruby 에서 `Tempfile`는 표준 라이브러리에서 제공합니다. 이것은 임시 파일을 만들고 자동으로 그것을 삭제하는 기능을 가지고 있습니다. `Tempfile`는 먼저 일반 파일처럼 작동하고, 사용 후에는 닫히고 삭제됩니다. 루비 버전 1.8.7부터 도입되어 개발자들 사이에서 널리 사용되었습니다. 대안으로 `StringIO`가 있지만, 이는 인-메모리에서만 작동하고 실제 파일 시스템에는 영향을 주지 않습니다. `Tempfile`이 내부적으로 사용하는 클래스에는 `File`과 `Dir` 라이브러리가 있습니다. 시스템에 따라 임시 파일이 저장되는 경로가 다를 수 있는데, 대체로 `/tmp` 폴더를 많이 사용합니다.

## See Also (참고자료)
- Understanding Ruby's `File` and `Dir` classes: [https://ruby-doc.org/core/File.html](https://ruby-doc.org/core/File.html), [https://ruby-doc.org/core/Dir.html](https://ruby-doc.org/core/Dir.html)
