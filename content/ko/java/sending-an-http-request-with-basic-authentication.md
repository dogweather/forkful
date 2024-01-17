---
title:                "기본 인증과 함께 http 요청 보내기"
html_title:           "Java: 기본 인증과 함께 http 요청 보내기"
simple_title:         "기본 인증과 함께 http 요청 보내기"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

HTTP 요청에 기본 인증을 사용하는 것은 인증된 사용자만이 액세스할 수 있는 보안이 강화된 웹 서비스를 만들기 위한 방법입니다. 프로그래머들은 이것을 사용하여 사용자가 자신의 어플리케이션에 로그인을 하도록 하거나 보안 인증이 필요한 API에 액세스하는 데 사용합니다.

# 방법:

```Java
// URL을 생성합니다.
URL url = new URL("https://example.com/api/user");

// HTTP Connection을 만듭니다.
HttpURLConnection connection = (HttpURLConnection) url.openConnection();

// 기본 인증을 설정합니다.
String username = "username";
String password = "password";
String encoded = Base64.getEncoder().encodeToString((username + ":" + password).getBytes());
connection.setRequestProperty("Authorization", "Basic " + encoded);

// 요청 메소드 및 헤더를 설정합니다.
connection.setRequestMethod("GET");
connection.setRequestProperty("Accept", "application/json");

// 요청 전송 및 응답 코드 확인
int responseCode = connection.getResponseCode();
System.out.println("Response Code: " + responseCode);

// 응답 메세지를 읽습니다.
BufferedReader reader = new BufferedReader(new InputStreamReader(connection.getInputStream()));
String output;
StringBuilder response = new StringBuilder();
while ((output = reader.readLine()) != null) {
    response.append(output);
}

// 응답 메세지 출력
System.out.println("Response: " + response.toString());

// 연결 종료
connection.disconnect();
```

예상 결과:
```
Response Code: 200
Response: {
    "id": "12345",
    "username": "username",
    "email": "username@example.com"
}
```

# 깊이 파고들기:

## 역사적 맥락:
기본 인증은 HTTP의 초기 버전에서 이미 사용되어 왔습니다. 이는 사용자의 인증 정보를 Base64로 인코딩하여 전송하는 간단한 방법으로, 웹의 보안을 위해 고안된 첫 번째 방식 중 하나입니다.

## 대안:
기본 인증은 매우 간단하지만 보안적으로 취약합니다. 따라서 현재는 보다 안전한 HTTPS를 사용하는 것이 권장되고 있습니다. 또 다른 대안으로는 OAuth나 JWT같은 보다 강력한 인증 시스템을 사용하는 것이 있습니다.

## 구현 세부사항:
기본 인증은 Base64로 인코딩되기 때문에 실제로 매우 취약합니다. 따라서 보안이 중요한 경우, HTTPS와 함께 사용하는 것이 좋습니다. 또한 인증 정보를 URL 매개 변수에 보내는 것보다는 헤더에 포함시키는 것이 더 안전합니다.

# 참고:

- [Java HTTP 요청 보내기 튜토리얼](https://www.baeldung.com/java-http-request)
- [Base64 인코딩과 디코딩](https://www.baeldung.com/java-base64-encode-and-decode)
- [HTTP 기본 인증에 대한 RFC](https://www.ietf.org/rfc/rfc2617.txt)