## FAA Aeronautical Reference Platform
Airports and Navaids on Supabase with API Access

**Status:** Draft  
**Version:** 0.1  
**Date:** 2025-12-17  
**Author:** Mike  
**Repository:** (private GitHub repo link placeholder)  
**Primary Data Source:** FAA NASR subscription (cycle-based)

---

## 1. Executive summary
This project provides a reusable backend platform that ingests publicly available FAA aeronautical reference data (initially airports and navaids), stores it in a Supabase-hosted Postgres database, and exposes it via a fast, stable read API. The platform is designed to support multiple downstream projects, including narrative-to-structured extraction workflows that require authoritative lookup of identifiers and coordinates. The system is built for low cost during early development and a clean migration path to AWS when scale, uptime, or performance requirements grow.

---

## 2. Problem statement
Downstream analytics and automation projects repeatedly need authoritative aeronautical reference data (airport identifiers, names, coordinates, navaid information). Storing full reference datasets inside individual analysis repositories increases local storage burden, complicates version control, and makes reuse difficult. A centralized database and API solves this by providing one authoritative reference source accessible across projects.

---

## 3. Goals
- Provide a single authoritative reference database for airports and navaids.
- Provide fast read access through a stable API for downstream tools (R, Python, web apps).
- Support cycle-based updates when FAA publishes new data.
- Keep write access limited to ingestion automation.
- Keep the design portable so it can move from Supabase to AWS Postgres without redesign.
