---
date: 2024-01-20 17:44:52.732974-07:00
description: "\uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD55C\uB2E4\
  \uB294 \uAC83\uC740 \uC778\uD130\uB137 \uC0C1\uC758 \uC815\uBCF4\uB97C \uB85C\uCEEC\
  \ \uCEF4\uD4E8\uD130\uB85C \uAC00\uC838\uC624\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uB370\uC774\uD130 \uBD84\uC11D, \uBAA8\uB2C8\
  \uD130\uB9C1, \uBC31\uC5C5 \uB610\uB294 \uC790\uB3D9\uD654\uB41C \uD14C\uC2A4\uD2B8\
  \uB97C \uC704\uD574 \uC774\uB97C \uC218\uD589\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.545058-06:00'
model: gpt-4-1106-preview
summary: "\uC6F9 \uD398\uC774\uC9C0\uB97C \uB2E4\uC6B4\uB85C\uB4DC\uD55C\uB2E4\uB294\
  \ \uAC83\uC740 \uC778\uD130\uB137 \uC0C1\uC758 \uC815\uBCF4\uB97C \uB85C\uCEEC \uCEF4\
  \uD4E8\uD130\uB85C \uAC00\uC838\uC624\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4."
title: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
weight: 42
---

## How to: (어떻게 하나요?)
```PowerShell
# Invoke-WebRequest를 사용하여 웹 페이지 내용을 다운로드합니다.
$response = Invoke-WebRequest -Uri "http://example.com"

# 웹 페이지의 HTML 내용을 파일로 저장합니다.
$response.Content > "example.html"

# 결과 확인
Get-Content -Path "example.html"
```
실행하면 'example.com'의 HTML 내용이 'example.html' 파일로 저장됩니다.

## Deep Dive (심층적인 분석)
과거에는 웹 페이지를 다운로드하기 위하여 `wget`이나 `curl`과 같은 독립적인 도구들이 많이 사용되었습니다. PowerShell이 등장하면서, `Invoke-WebRequest`나 `Invoke-RestMethod` 같은 명령어를 통해 직접 HTTP 요청을 보내고 응답을 받을 수 있게 되었습니다. 이들은 RESTful API 호출에도 사용되기 때문에 웹 자동화와 API 통합 두 분야에서 모두 유용합니다. 대안으로는 .NET Framework의 클래스를 사용할 수 있지만, PowerShell 명령어는 사용이 더 간단합니다.

## See Also (참고 자료)
- [Invoke-WebRequest Documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [PowerShell Gallery](https://www.powershellgallery.com/)
