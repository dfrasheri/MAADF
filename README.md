# MAADF
# StreamlineProcure: Intelligent Procurement Automation Platform

## Overview

StreamlineProcure is a modular procurement form system designed to optimize enterprise procurement workflows through automation, document intelligence, and real-time system integration. The platform enables intelligent parsing and evaluation of procurement data using machine learning and OCR, while interfacing directly with NetSuite ERP to ensure seamless synchronization of vendor and product information.

This project focuses on accelerating product intake, reducing manual overhead, and improving data accuracy across procurement operations.

## Key Features

- **ML-Powered Evaluation**: 
  Utilizes a TF-IDF-based model for semantic matching, supplier scoring, and auto-classification of procurement line items.
  
- **Advanced OCR**: 
  Integrates both Tesseract OCR and AWS Textract to extract structured and unstructured data from scanned PDFs, receipts, and physical invoices.

- **ERP Integration**: 
  Bi-directional synchronization with NetSuite ERP for real-time access to vendor records, pricing history, and item master data.

- **Document Intelligence**: 
  Automated extraction and tokenization of procurement documents using NLP pipelines for entity recognition and field inference.

- **Form Streamlining**:
  Dynamically generates procurement forms based on previously inferred data, minimizing user input requirements.

## Architecture

- **Frontend**: React.js or Vue.js SPA with dynamic form components.
- **Backend**: Flask / FastAPI microservices with RESTful APIs.
- **ML Module**: Scikit-learn-based TF-IDF vectorizer pipeline trained on labeled procurement data.
- **OCR Engines**: 
  - `pytesseract` (Tesseract OCR)
  - `boto3` for AWS Textract
- **ERP Connector**: NetSuite SuiteTalk (SOAP/XML) or REST integration layer for CRUD operations on items, POs, and vendors.

## Setup

### Prerequisites

- Python 3.9+
- Node.js 16+ (for frontend)
- AWS credentials (for Textract)
- NetSuite sandbox environment (for API testing)
- Tesseract installed and available on system path

### Installation

```bash
git clone https://github.com/your-org/streamlineprocure.git
cd streamlineprocure
pip install -r requirements.txt
