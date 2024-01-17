---
title:                "http 요청 보내기"
html_title:           "PowerShell: http 요청 보내기"
simple_title:         "http 요청 보내기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

HTTP 요청을 보내는 것은 웹상에서 데이터를 교환하기 위해 사용되는 일반적인 방법입니다. 이는 웹페이지를 불러오는 것부터 파일 업로드, API 호출까지 다양한 목적으로 사용될 수 있습니다. 프로그래머들은 HTTP 요청을 보내는 것으로 웹 상의 다양한 데이터를 쉽고 빠르게 처리할 수 있기 때문에 많이 사용합니다.

## 하는 법:

### 기본적인 HTTP 요청

```PowerShell
# Invoke-WebRequest 명령어를 사용합니다.
Invoke-WebRequest -Uri "https://www.example.com" 

# 결과는 일련의 데이터를 반환합니다.
# 예:
# StatusCode        : 200
# StatusDescription : OK
# Content           : <!DOCTYPE html>
# Headers           : {[Transfer-Encoding, chunked], [Connection, keep-alive], [Content-Encoding, gzip], [Cache-
#                     Control, no-cache]
# RawContent        : HTTP/1.1 200 OK
#                     Transfer-Encoding: chunked
#                     Connection: keep-alive
#                     Content-Encoding: gzip
#                     Cache-Control: no-cache
#
#                     <!DOCTYPE html>
#                     ...
```

### 쿼리스트링과 요청 바디

쿼리스트링이나 요청 바디를 포함하는 HTTP 요청도 동일한 방식으로 보낼 수 있습니다. 또한 파라미터도 함께 지정할 수 있습니다.

```PowerShell
# 쿼리스트링에 파라미터 추가하기
Invoke-WebRequest -Uri "https://www.example.com" -Method GET -QueryString @{"param1"="value1"; "param2"="value2"}

# 요청 바디에 JSON 데이터 추가하기
Invoke-WebRequest -Uri "https://www.example.com" -Method POST -Body '{"key": "value"}' -ContentType "application/json"
```

### 서버가 인증을 요구하는 경우

HTTP 요청을 보내는 경우, 서버가 인증을 요구할 수도 있습니다. 이 경우, Credential 매개변수를 통해 사용자 이름과 비밀번호를 지정할 수 있습니다.

```PowerShell
# 인증 정보 추가하기
Invoke-WebRequest -Uri "https://www.example.com" -Credential (Get-Credential)
```

## 심화 분석:

### 배경 정보

HTTP 요청은 1991년에 최초로 도입된 이래로 계속 발전해오면서 웹 기반 시스템과 애플리케이션에서 매우 중요한 역할을 합니다. 현재 웹 API와 클라이언트 간 통신을 위한 사실상의 표준 방식으로 자리 잡았으며, 대부분의 프로그래밍 언어에서 이를 지원합니다.

### 대안

PowerShell 이외에도 HTTP 요청을 보낼 수 있는 다양한 방법이 있습니다. 예를 들어, cURL이나 Postman 등의 다른 도구를 사용할 수 있습니다. 하지만 PowerShell은 이미 Windows 운영체제의 일부분이기 때문에 추가 프로그램 없이도 바로 사용할 수 있어 편리합니다.

### 구현 세부 정보

HTTP 요청을 보내는 방법은 다양하지만 주로 사용되는 방식은 Invoke-WebRequest cmdlet를 사용하는 것입니다. 이 cmdlet은 .NET Framework에서 제공하는 WebRequest 클래스를 기반으로 동작하며, .NET의 HttpWebRequest 라이브러리를 사용합니다.

## 관련 링크:

- [PowerShell 실전: Invoke-WebRequest로 API 호출하기](https://duffney.io/invoke-webrequest)
- [PowerShell Documentation: Invoke-WebRequest cmdlet](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [cURL 공식 사이트](https://curl.se/)
- [Postman 공식 사이트](https://www.postman.com/)