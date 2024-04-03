---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:30.493722-07:00
description: "Wie macht man das: Rubys Standardbibliothek enth\xE4lt die Klassen `Date`\
  \ und `Time` zur Handhabung von Datums- und Zeitangaben. So k\xF6nnen Sie das aktuelle\u2026"
lastmod: '2024-03-13T22:44:54.410761-06:00'
model: gpt-4-0125-preview
summary: "Rubys Standardbibliothek enth\xE4lt die Klassen `Date` und `Time` zur Handhabung\
  \ von Datums- und Zeitangaben."
title: Den aktuellen Datum abrufen
weight: 29
---

## Wie macht man das:
Rubys Standardbibliothek enthält die Klassen `Date` und `Time` zur Handhabung von Datums- und Zeitangaben. So können Sie das aktuelle Datum abrufen:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

Beispielausgabe: 
```
2023-04-12
```

Um die Zeit zusammen mit dem Datum einzuschließen, ist Rubys `Time`-Klasse besser geeignet:

```ruby
current_time = Time.now
puts current_time
```

Beispielausgabe: 
```
2023-04-12 14:33:07 +0200
```

Wenn Sie mehr Funktionalität benötigen, wie z.B. Zeitmanagement für Zeitzonen, möchten Sie vielleicht ein Drittanbieter-Gem wie `ActiveSupport` verwenden (Teil von Rails, kann aber eigenständig genutzt werden).

Fügen Sie zunächst `activesupport` zu Ihrem Gemfile hinzu und führen Sie `bundle install` aus:

```ruby
gem 'activesupport'
```

Verwenden Sie es dann, um Zeitzonen zu verwalten:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # Setzen Sie Ihre gewünschte Zeitzone
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

Beispielausgabe:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
