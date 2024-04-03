---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:51.040048-07:00
description: "Jak to zrobi\u0107: Bezpo\u015Brednia praca z YAML w Bashu wymaga nieco\
  \ pomys\u0142owo\u015Bci, poniewa\u017C Bash nie ma wbudowanego wsparcia dla parsowania\
  \ YAML. Jednak mo\u017Cesz\u2026"
lastmod: '2024-03-13T22:44:35.605677-06:00'
model: gpt-4-0125-preview
summary: "Bezpo\u015Brednia praca z YAML w Bashu wymaga nieco pomys\u0142owo\u015B\
  ci, poniewa\u017C Bash nie ma wbudowanego wsparcia dla parsowania YAML."
title: Praca z YAML
weight: 41
---

## Jak to zrobić:
Bezpośrednia praca z YAML w Bashu wymaga nieco pomysłowości, ponieważ Bash nie ma wbudowanego wsparcia dla parsowania YAML. Jednak możesz użyć zewnętrznych narzędzi, takich jak `yq` (lekkie i przenośne narzędzie do wiersza poleceń dla YAML), aby efektywnie współdziałać z plikami YAML. Przejdźmy przez kilka typowych operacji:

### Instalacja `yq`:
Zanim zagłębimy się w przykłady, upewnij się, że masz zainstalowany `yq`. Zwykle możesz go zainstalować z menedżera pakietów, na przykład na Ubuntu:

```bash
sudo apt-get install yq
```

Lub możesz pobrać go bezpośrednio z repozytorium GitHub.

### Odczytywanie wartości:
Załóżmy, że masz plik o nazwie `config.yaml` z następującą zawartością:

```yaml
database:
  host: localhost
  port: 5432
user:
  name: admin
  password: secret
```

Aby odczytać hosta bazy danych, możesz użyć `yq` w następujący sposób:

```bash
yq e '.database.host' config.yaml
```

**Przykładowy wynik:**

```
localhost
```

### Aktualizowanie wartości:
Aby zaktualizować nazwę użytkownika w `config.yaml`, użyj polecenia `yq eval` z opcją `-i` (in-place):

```bash
yq e '.user.name = "newadmin"' -i config.yaml
```

Zweryfikuj zmianę za pomocą:

```bash
yq e '.user.name' config.yaml
```

**Przykładowy wynik:**

```
newadmin
```

### Dodawanie nowego elementu:
Aby dodać nowy element w sekcji bazy danych, jak nowe pole `timeout`:

```bash
yq e '.database.timeout = 30' -i config.yaml
```

Sprawdzenie zawartości pliku potwierdzi dodanie.

### Usuwanie elementu:
Aby usunąć hasło użytkownika:

```bash
yq e 'del(.user.password)' -i config.yaml
```

Ta operacja usunie pole hasła z konfiguracji.

Pamiętaj, `yq` jest potężnym narzędziem i ma wiele więcej możliwości, w tym konwersję YAML na JSON, łączenie plików i nawet bardziej złożone manipulacje. Zapoznaj się z dokumentacją `yq` dla dalszych poszukiwań.
