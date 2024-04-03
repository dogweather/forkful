---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:51.547578-07:00
description: "\uBC29\uBC95: Bash\uC5D0\uC11C YAML\uC744 \uC9C1\uC811 \uB2E4\uB8E8\uAE30\
  \ \uC704\uD574\uC11C\uB294 Bash\uAC00 YAML \uD30C\uC2F1\uC744 \uB0B4\uC7A5 \uC9C0\
  \uC6D0\uD558\uC9C0 \uC54A\uAE30 \uB54C\uBB38\uC5D0 \uC57D\uAC04\uC758 \uC7AC\uCE58\
  \uAC00 \uD544\uC694\uD569\uB2C8\uB2E4. \uADF8\uB7EC\uB098 `yq`(\uACBD\uB7C9\uC774\
  \uBA70 \uD734\uB300\uAC00\uB2A5\uD55C \uCEE4\uB9E8\uB4DC\uB77C\uC778 YAML \uD504\
  \uB85C\uC138\uC11C)\uC640 \uAC19\uC740 \uC678\uBD80 \uB3C4\uAD6C\uB97C \uC0AC\uC6A9\
  \uD558\uC5EC \uD6A8\uC728\uC801\uC73C\uB85C YAML \uD30C\uC77C\uACFC \uC0C1\uD638\
  \ \uC791\uC6A9\uD560 \uC218\u2026"
lastmod: '2024-03-13T22:44:55.511281-06:00'
model: gpt-4-0125-preview
summary: "Bash\uC5D0\uC11C YAML\uC744 \uC9C1\uC811 \uB2E4\uB8E8\uAE30 \uC704\uD574\
  \uC11C\uB294 Bash\uAC00 YAML \uD30C\uC2F1\uC744 \uB0B4\uC7A5 \uC9C0\uC6D0\uD558\uC9C0\
  \ \uC54A\uAE30 \uB54C\uBB38\uC5D0 \uC57D\uAC04\uC758 \uC7AC\uCE58\uAC00 \uD544\uC694\
  \uD569\uB2C8\uB2E4."
title: "YAML\uB85C \uC791\uC5C5\uD558\uAE30"
weight: 41
---

## 방법:
Bash에서 YAML을 직접 다루기 위해서는 Bash가 YAML 파싱을 내장 지원하지 않기 때문에 약간의 재치가 필요합니다. 그러나 `yq`(경량이며 휴대가능한 커맨드라인 YAML 프로세서)와 같은 외부 도구를 사용하여 효율적으로 YAML 파일과 상호 작용할 수 있습니다. 몇 가지 일반적인 작업을 살펴봅시다:

### `yq` 설치:
예시를 다루기 전에, `yq`가 설치되어 있는지 확인하세요. 보통 패키지 매니저에서 설치할 수 있습니다. 예를 들어, 우분투에서는:

```bash
sudo apt-get install yq
```

또는 GitHub 저장소에서 직접 다운로드할 수 있습니다.

### 값을 읽기:
`config.yaml`이라는 파일이 있고 다음과 같은 내용이 있다고 가정해 보겠습니다:

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secret
```

데이터베이스 호스트를 읽으려면 다음과 같이 `yq`를 사용할 수 있습니다:

```bash
yq e '.database.host' config.yaml
```

**샘플 출력:**

```
localhost
```

### 값을 업데이트하기:
`config.yaml`에서 사용자 이름을 업데이트하려면 `-i` (in-place) 옵션을 사용하여 `yq eval` 명령을 사용하세요:

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

다음으로 변경 사항을 확인하세요:

```bash
yq e '.user.name' config.yaml
```

**샘플 출력:**

```
newadmin
```

### 새 요소 추가하기:
데이터베이스 섹션 아래에 새 필드 `timeout`처럼 새 요소를 추가하려면:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

파일의 내용을 확인하면 추가가 확인됩니다.

### 요소 삭제하기:
사용자 아래의 비밀번호를 제거하려면:

```bash
yq e 'del(.user.password)' -i config.yaml
```

이 작업은 구성에서 비밀번호 필드를 제거할 것입니다.

기억하세요, `yq`는 매우 강력한 도구이며, YAML을 JSON으로 변환, 파일 병합, 그리고 더 복잡한 조작을 포함하여 많은 기능을 가지고 있습니다. 더 많은 탐구를 위해 `yq` 문서를 참조하세요.
