---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

텍스트 파일 작성은 데이터를 저장하고 로그, 설정, 보고서를 기록하기 위해 프로그래머들이 사용합니다. 간단, 자동화, 공유용으로 중요합니다.

## How to (방법):

```PowerShell
# 파일에 텍스트 쓰기
"안녕하세요, 여러분!" | Out-File -FilePath "hello.txt"
# 샘플 출력 없음, 생성된 "hello.txt" 파일을 확인하세요.

# 파일에 내용 추가하기
"파워쉘 사용법에 대해 배웁시다." | Add-Content -Path "hello.txt"

# 파일 읽기
Get-Content -Path "hello.txt"
```

## Deep Dive (심층 분석):

1980년대, 텍스트 파일은 데이터 교환의 근간이 되었습니다. XML, JSON 등 새로운 형식이 떠오르고 있지만, 텍스트 파일의 단순성과 범용성은 그대로 중요합니다. PowerShell에서는 `Out-File`, `Add-Content`, `Set-Content` 등의 명령어로 다양한 방식으로 파일에 쓸 수 있으며, 인코딩이나 퍼포먼스 옵션을 조정할 수 있습니다.

## See Also (참고 자료):

- [Microsoft 공식 문서: Set-Content](https://docs.microsoft.com/ko-kr/powershell/module/Microsoft.PowerShell.Management/Set-Content)
- [Microsoft 공식 문서: Add-Content](https://docs.microsoft.com/ko-kr/powershell/module/Microsoft.PowerShell.Management/Add-Content)
