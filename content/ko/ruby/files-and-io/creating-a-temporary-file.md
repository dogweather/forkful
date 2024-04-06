---
date: 2024-01-20 17:41:17.668707-07:00
description: "How to: (\uBC29\uBC95) Ruby \uC5D0\uC11C `Tempfile`\uB294 \uD45C\uC900\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uC5D0\uC11C \uC81C\uACF5\uD569\uB2C8\uB2E4. \uC774\
  \uAC83\uC740 \uC784\uC2DC \uD30C\uC77C\uC744 \uB9CC\uB4E4\uACE0 \uC790\uB3D9\uC73C\
  \uB85C \uADF8\uAC83\uC744 \uC0AD\uC81C\uD558\uB294 \uAE30\uB2A5\uC744 \uAC00\uC9C0\
  \uACE0 \uC788\uC2B5\uB2C8\uB2E4. `Tempfile`\uB294 \uBA3C\uC800 \uC77C\uBC18 \uD30C\
  \uC77C\uCC98\uB7FC \uC791\uB3D9\uD558\uACE0, \uC0AC\uC6A9 \uD6C4\uC5D0\uB294 \uB2EB\
  \uD788\uACE0 \uC0AD\uC81C\uB429\uB2C8\uB2E4. \uB8E8\uBE44 \uBC84\uC804 1.8.7\uBD80\
  \uD130\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:10.189789-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) Ruby \uC5D0\uC11C `Tempfile`\uB294 \uD45C\uC900 \uB77C\uC774\
  \uBE0C\uB7EC\uB9AC\uC5D0\uC11C \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uC784\uC2DC \uD30C\uC77C \uC0DD\uC131\uD558\uAE30"
weight: 21
---

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
